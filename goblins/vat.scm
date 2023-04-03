;;; Copyright 2021-2022 Christine Lemmer-Webber
;;; Copyright 2022 Jessica Tallon
;;; Copyright 2023 David Thompson
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(define-module (goblins vat)
  #:use-module (goblins base-io-ports)
  #:use-module (goblins core)
  #:use-module (goblins inbox)
  #:use-module (goblins default-vat-scheduler)
  #:use-module (goblins utils random-name)
  #:use-module (goblins utils ring-buffer)
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (vat-event?
            vat-send-event?
            vat-receive-event?
            vat-event-listen?
            vat-event-message?
            vat-event-local?
            vat-event-type
            vat-event-churn
            vat-event-timestamp
            vat-event-far-timestamp
            vat-event-message
            vat-event-snapshot
            vat-event-connector
            vat-event-previous
            vat-event-next
            vat-event-trace
            vat-event-tree

            &vat-turn-error
            vat-turn-error-event

            vat-envelope?
            vat-envelope-message
            vat-envelope-timestamp
            vat-envelope-return?

            all-vats
            lookup-vat
            make-vat
            vat?
            vat-id
            vat-name
            vat-connector
            vat-clock
            vat-log-capacity
            vat-log-length
            vat-log-ref
            vat-log-ref-by-time
            vat-log-ref-by-message
            vat-log-ref-previous
            vat-log-ref-next
            vat-log-error-for-event
            vat-log-errors
            vat-log-resize!
            vat-log-clear!
            set-vat-logging!
            vat-running?
            vat-logging?
            vat-halt!
            vat-start!
            call-with-vat
            with-vat
            make-fibrous-vat
            spawn-fibrous-vat
            spawn-vat

            syscaller-free-fiber
            spawn-fibrous-vow
            fibrous

            define-vat-run

            ;; and here's a hack, but maybe someone wants
            ;; to start with it and tweak it
            port-redirect-dynamic-wrap))

;; Vats
;; ----

;;;                .=======================.
;;;                |Internal Vat Schematics|
;;;                '======================='
;;;
;;;             stack           heap
;;;              ($)         (actormap)
;;;           .-------.----------------------. -.
;;;           |       |                      |  |
;;;           |       |   .-.                |  |
;;;           |       |  (obj)         .-.   |  |
;;;           |       |   '-'         (obj)  |  |
;;;           |  __   |                '-'   |  |
;;;           | |__>* |          .-.         |  |- actormap
;;;           |  __   |         (obj)        |  |  territory
;;;           | |__>* |          '-'         |  |
;;;           |  __   |                      |  |
;;;           | |__>* |                      |  |
;;;           :-------'----------------------: -'
;;;     queue |  __    __    __              | -.
;;;      (<-) | |__>* |__>* |__>*            |  |- event loop
;;;           '------------------------------' -'  territory
;;;
;;;
;;; Finished reading core.rkt and thought "gosh what I want more out of
;;; life is more ascii art diagrams"?  Well this one is pretty much figure
;;; 14.2 from Mark S. Miller's dissertation (with some Goblins specific
;;; modifications):
;;;   http://www.erights.org/talks/thesis/
;;;
;;; If we just look at the top of the diagram, we can look at the world as
;;; it exists purely in terms of actormaps.  The right side is the actormap
;;; itself, effectively as a "heap" of object references mapped to object
;;; behavior (not unlike how in memory-unsafe languages pointers map into
;;; areas of memory).  The left-hand side is the execution of a
;;; turn-in-progress... the bottom stubby arrow corresponds to the initial
;;; invocation against some actor in the actormap, and stacked on top are
;;; calls to other actors via immediate call-return behavior using $.
;;;
;;; Vats come in when we add the bottom half of the diagram: the event
;;; loop!  An event loop manages a queue of messages that are to be handled
;;; asynchronously... one after another after another.  Each message which
;;; is handled gets pushed onto the upper-left hand stack, executes, and
;;; bottoms out in some result (which the vat then uses to resolve any
;;; promise that is waiting on this message).  During its execution, this
;;; might result in building up more messages by calls sent to <-, which,
;;; if to refrs in the same vat, will be put on the queue (FIFO order), but
;;; if they are in another vat will be sent there using the reference's vat
;;; or machine connector (depending on if local/remote).
;;;
;;; Anyway, you could implement a vat-like event loop yourself, but this
;;; module implements the general behavior.  The most important thing if
;;; you do so is to resolve promises based on turn result and also
;;; implement the vat-connnector behavior (currently the handle-message
;;; and vat-id methods, though it's not unlikely this module will get
;;; out of date... oops)

;; The purpose of this is to prevent issues where a user hacking
;; with Geiser's buffer evaluation commands (eg C-x C-e)
;; launches a vat, and things weirdly break... because geiser
;; sets redirects output so that it can capture it to display to the
;; user when hacking that way, but those ports are closed at the
;; end of the evaulation.  But since the vat would run in its own
;; fiber/thread, any attempts to write to output/error ports would
;; throw an exception.  This redirects them "back".
(define (port-redirect-dynamic-wrap proc)
  (parameterize ((current-output-port %base-output-port)
                 (current-error-port %base-error-port))
    (proc)))

;; Vat event logging
;; =================

(define-record-type <vat-event>
  (make-vat-event type churn timestamp far-timestamp message snapshot)
  vat-event?
  (type vat-event-type) ; either 'send' or 'receive'
  (churn vat-event-churn)
  (timestamp vat-event-timestamp)
  (far-timestamp vat-event-far-timestamp)
  (message vat-event-message)
  (snapshot vat-event-snapshot))

(define (print-vat-event event port)
  (format port
          "#<vat-event type: ~a timestamp: ~a far-timestamp: ~a message: ~a>"
          (vat-event-type event)
          (vat-event-timestamp event)
          (vat-event-far-timestamp event)
          (vat-event-message event)))

(set-record-type-printer! <vat-event> print-vat-event)

(define (vat-send-event? event)
  "Return #t if EVENT is a send event."
  (and (vat-event? event) (eq? (vat-event-type event) 'send)))

(define (vat-receive-event? event)
  "Return #t if EVENT is a receive event."
  (and (vat-event? event) (eq? (vat-event-type event) 'receive)))

(define (vat-event-listen? event)
  "Return #t if EVENT is for a listen request."
  (listen-request? (vat-event-message event)))

(define (vat-event-message? event)
  "Return #t if EVENT is for a message."
  (message? (vat-event-message event)))

(define (vat-event-local? event)
  "Return #t if EVENT is for a local message."
  (local-object-refr?
   (message-or-request-to (vat-event-message event))))

(define (vat-event-connector event)
  "Return the connector for the vat that EVENT belongs to. Send events
belong to the sender.  Receive events belong to the receiver."
  (let ((type (vat-event-type event))
        (msg (vat-event-message event)))
    (if (eq? type 'send)
        (message-or-request-from-vat msg)
        (local-refr-vat-connector
         (message-or-request-to msg)))))

(define (vat-event-previous event)
  "Return the event that happened before EVENT, either in the same churn
or, in the case of a receive event from another vat, the corresponding
send event.  #f is returned if no such event is found."
  (let ((far-timestamp (vat-event-far-timestamp event))
        (vat-connector (message-or-request-from-vat
                        (vat-event-message event))))
    (if far-timestamp
        (vat-connector 'find-event-by-time far-timestamp)
        (vat-connector 'find-previous-event event))))

(define (vat-event-next event)
  "Return the events that happened after EVENT in the same churn, or #f
if there are no such events."
  (let ((vat-connector (vat-event-connector event)))
    (vat-connector 'find-next-events event)))

(define (vat-event-trace event)
  "Return a list of events, starting with EVENT, and working back
through previous events until a root event is reached or there is no
more history to search.  Much like how backtraces are linear slices of
call stacks, vat traces are linear slices of the event graph."
  (if (vat-event? event)
      (cons event (vat-event-trace (vat-event-previous event)))
      '()))

(define (vat-event-tree event)
  "Return a tree of events that lead up to EVENT."
  (define (build-event-tree root)
    ;; Receive events may represent a leaf node if they did not
    ;; asynchronously invoke any other actors.  Otherwise, create
    ;; sub-trees for each additional event.
    (if (vat-receive-event? root)
        (match (vat-event-next root)
          (() root)
          (next-events
           (cons root (map build-event-tree next-events))))
        ;; Send events require talking to another vat and building a
        ;; sub-tree.
        (let* ((msg (vat-event-message root))
               ;; Get the vat connector that the message was sent to.
               (vat-connector (local-refr-vat-connector
                               (message-or-request-to msg)))
               ;; The message is the only context we have to search
               ;; by, so that's what we do.
               ;;
               ;; "I *know* I sent you this message, now I need to
               ;; know what happened once you got it!"
               (far-event (vat-connector 'find-event-by-message msg)))
          (if far-event
              ;; Recur on the far event to build a sub-tree.
              (cons root (list (build-event-tree far-event)))
              ;; The other vat is either not logging or no longer has
              ;; logs for this event, so we've disappointingly reached
              ;; a leaf node.
              root))))
  ;; Find the root by getting the last event in the trace.  The *last*
  ;; event in the trace is our *first* event because vat-event-trace
  ;; returns events in backtrace style using reverse chronological
  ;; order.
  (match (vat-event-trace event)
    ((_ ... root)
     ;; Build a tree starting from the root.
     (build-event-tree root))))

;; The vat log maintains a finite amount of history about messages
;; that have been sent/received in the vat.  These events are indexed
;; for easy querying in a variety of situations.
(define-record-type <vat-log>
  (%make-vat-log events time-index message-index prev-index next-index
                 error-index mutex)
  vat-log?
  (events vat-log-events)
  (time-index vat-log-time-index)
  (message-index vat-log-message-index)
  (prev-index vat-log-prev-index)
  (next-index vat-log-next-index)
  (error-index vat-log-error-index)
  (mutex vat-log-mutex))

(define (make-vat-log max-length)
  (%make-vat-log (make-ring-buffer max-length)
                 (make-hash-table)
                 (make-hash-table)
                 (make-hash-table)
                 (make-hash-table)
                 (make-hash-table)
                 (make-mutex)))

(define (vat-log-delete-from-index! log event)
  (let ((time-index (vat-log-time-index log))
        (message-index (vat-log-message-index log))
        (prev-index (vat-log-prev-index log))
        (next-index (vat-log-next-index log))
        (error-index (vat-log-error-index log)))
    (hashv-remove! time-index (vat-event-timestamp event))
    (hashq-remove! message-index (vat-event-message event))
    (hashq-remove! prev-index event)
    (hashq-remove! next-index event)
    (hashq-remove! error-index event)))

(define (%vat-log-resize! log capacity)
  (with-mutex (vat-log-mutex log)
    ;; If the log size is shrinking, we need to delete indexed events
    ;; for the items that no longer fit.
    (let ((n (max (- (%vat-log-length log) capacity) 0)))
      (let loop ((i 0))
        (when (< i n)
          (vat-log-delete-from-index! log (%vat-log-ref log i))))
      (ring-buffer-resize! (vat-log-events log) capacity))))

(define (%vat-log-clear! log)
  (with-mutex (vat-log-mutex log)
    (ring-buffer-clear! (vat-log-events log))
    (hash-clear! (vat-log-time-index log))
    (hash-clear! (vat-log-message-index log))
    (hash-clear! (vat-log-prev-index log))
    (hash-clear! (vat-log-next-index log))
    (hash-clear! (vat-log-error-index log))))

(define (%vat-log-append! log event prev)
  (with-mutex (vat-log-mutex log)
    (let ((events (vat-log-events log))
          (time-index (vat-log-time-index log))
          (message-index (vat-log-message-index log))
          (prev-index (vat-log-prev-index log))
          (next-index (vat-log-next-index log))
          (error-index (vat-log-error-index log)))
      ;; Remove indexed events as they are expired from the ring buffer.
      (when (ring-buffer-full? events)
        (vat-log-delete-from-index! log (ring-buffer-ref events 0)))
      (ring-buffer-put! events event)
      (hashv-set! time-index (vat-event-timestamp event) event)
      (hashq-set! message-index (vat-event-message event) event)
      (hashq-set! prev-index event prev)
      (hashq-set! next-index prev
                  (cons event (hashq-ref next-index prev '()))))))

(define (%vat-log-error! log event exception)
  (with-mutex (vat-log-mutex log)
    (hashq-set! (vat-log-error-index log) event exception)))

(define (%vat-log-error-for-event log event)
  (hashq-ref (vat-log-error-index log) event))

(define (%vat-log-errors log)
  (hash-map->list cons (vat-log-error-index log)))

(define (%vat-log-capacity log)
  (ring-buffer-capacity (vat-log-events log)))

(define (%vat-log-length log)
  (ring-buffer-length (vat-log-events log)))

(define (%vat-log-ref log i)
  (ring-buffer-ref (vat-log-events log) i))

(define (%vat-log-ref-by-time log t)
  (hashv-ref (vat-log-time-index log) t))

(define (%vat-log-ref-by-message log msg)
  (hashq-ref (vat-log-message-index log) msg))

(define (%vat-log-ref-previous log event)
  (hashq-ref (vat-log-prev-index log) event))

(define (%vat-log-ref-next log event)
  ;; Events are stored in reverse order in which they were processed.
  (reverse (hashq-ref (vat-log-next-index log) event '())))

;; Vats
;; ====

(define &vat-turn-error
  (make-exception-type '&vat-turn-error &error '(event)))

(define make-vat-turn-error (record-constructor &vat-turn-error))

(define vat-turn-error-event
  (exception-accessor &vat-turn-error
                      (record-accessor &vat-turn-error 'event)))

;; Vat envelopes contain a message, are postmarked with a Lamport
;; timestamp to indicate when it was sent, and have a flag that
;; indicates if the sender wants a reply.  Currently, the return? flag
;; is only used to support returning values to the user via
;; call-with-vat.
(define-record-type <vat-envelope>
  (make-vat-envelope message timestamp return?)
  vat-envelope?
  (message vat-envelope-message)
  (timestamp vat-envelope-timestamp)
  (return? vat-envelope-return?))

(define-record-type <vat>
  (%make-vat id name actormap running connector current-churn
             clock logging? log start-proc halt-proc send-proc)
  vat?
  (id vat-id)
  (name vat-name)
  (actormap vat-actormap)
  (running vat-running)
  (connector vat-connector)
  (current-churn vat-current-churn set-vat-current-churn!)
  (clock %vat-clock)
  (logging? %vat-logging?)
  (log vat-log)
  (start-proc vat-start-proc)
  (halt-proc vat-halt-proc)
  (send-proc vat-send-proc))

(define (print-vat vat port)
  (format port "#<vat id: ~a name: ~a>"
          (vat-id vat) (vat-name vat)))

(set-record-type-printer! <vat> print-vat)

;; A global table of vats keyed by id.
(define *vats* (make-weak-value-hash-table))

(define (all-vats)
  (hash-map->list (lambda (k v) v) *vats*))

(define (lookup-vat id)
  (hashv-ref *vats* id))

(define register-vat!
  (let ((mutex (make-mutex)))
    (lambda (vat)
      (with-mutex mutex
        (hashv-set! *vats* (vat-id vat) vat)))))

;; A global id counter for vats.
(define *vat-id-counter* (make-atomic-box 0))

(define (next-vat-id)
  (let* ((id (atomic-box-ref *vat-id-counter*)))
    ;; If the atomic box was updated in another thread then the id
    ;; we've just generated is no good and the counter will not be
    ;; updated.  Loop until we get a good one.
    (if (eq? (atomic-box-compare-and-swap! *vat-id-counter* id (+ id 1)) id)
        id
        (next-vat-id))))

(define default-log-capacity 256)

(define* (make-vat #:key name start halt send log? (log-capacity default-log-capacity))
  "Return a new vat named NAME.  Vat behavior is determined by three
event hooks:

START: A procedure that starts the vat process, presumably in a new
thread or other non-blocking manner. Accepts one argument: a procedure
which takes a message as its one argument and churns the underlying
actormap for the vat.

HALT: A thunk that stops the vat process.

SEND: A procedure that accepts a message to handle within the vat
process and a boolean flag indicating if the message result needs to
be returned to the sender or not.

If LOG? is #t, event logging is enabled.  By default, logging is
disabled.  LOG-CAPACITY events will be retained in the log."
  (define (connector . args)
    (match args
      (('name) (vat-name vat))
      (('handle-message timestamp msg)
       ;; TODO: We should indicate to the procedure which calls this that
       ;; the attempt to send the message failed... so, return an 'ok
       ;; or 'failed message here?
       (when (atomic-box-ref running?)
         (vat-send vat (make-vat-envelope msg timestamp #f))))
      ;; Event log queries.
      (('find-event-by-time timestamp)
       (vat-log-ref-by-time vat timestamp))
      (('find-event-by-message msg)
       (vat-log-ref-by-message vat msg))
      (('find-previous-event event)
       (vat-log-ref-previous vat event))
      (('find-next-events event)
       (vat-log-ref-next vat event))))
  (define am (make-actormap #:vat-connector connector))
  (define id (next-vat-id))
  (define clock (make-atomic-box 0))
  (define running? (make-atomic-box #f))
  (define logging? (make-atomic-box log?))
  (define log (make-vat-log log-capacity))
  (define index (make-hash-table))
  (define vat
    (%make-vat id name am running? connector 0 clock logging? log
               start halt send))
  (register-vat! vat)
  vat)

(define (vat-running? vat)
  "Return #t if VAT is currently running."
  (atomic-box-ref (vat-running vat)))

(define (vat-clock vat)
  (atomic-box-ref (%vat-clock vat)))

(define* (vat-next-timestamp vat #:optional (min-time 0))
  (let* ((clock (%vat-clock vat))
         (current-time (atomic-box-ref clock))
         (next-time (+ (max current-time min-time) 1))
         (prev-time (atomic-box-compare-and-swap! clock
                                                  current-time
                                                  next-time)))
    ;; It is possible that another thread has updated the counter
    ;; between getting the current value and attempting to increment
    ;; it.  If that is the case then try again until we succeed.
    (if (eq? current-time prev-time)
        next-time
        ;; Spin until we get our timestamp!
        (vat-next-timestamp vat min-time))))

(define (vat-next-churn-id vat)
  (let ((id (+ (vat-current-churn vat) 1)))
    (set-vat-current-churn! vat id)
    id))

(define (vat-halt! vat)
  "Stop processing turns for VAT."
  (atomic-box-set! (vat-running vat) #f)
  ((vat-halt-proc vat)))

(define* (vat-churn vat msg sent-at)
  (define churn-id (vat-next-churn-id vat))
  (define near-q (make-q))
  (define far-q (make-q))
  (define am (vat-actormap vat))
  (define snapshot (copy-whactormap am))
  (define new-am (make-transactormap am))
  (define this-vat-connector (actormap-vat-connector am))
  (define (near-msg? msg)
    (define to-refr (message-or-request-to msg))
    (and (local-refr? to-refr)
         (eq? (local-refr-vat-connector to-refr)
              this-vat-connector)))
  (define (queue-messages-appropriately! event msgs)
    (match msgs
      (() 'done)
      ((msg next-msgs ...)
       (queue-messages-appropriately! event next-msgs) ; last message first
       (if (near-msg? msg)
           (enq! near-q (list event msg))
           (enq! far-q (list event msg))))))
  (define (turn event)
    (define msg (vat-event-message event))
    (define-values (result buffer-am new-msgs)
      (actormap-turn-message new-am msg #:catch-errors? #t))
    (queue-messages-appropriately! event new-msgs)
    (match result
      (#('ok _)
       (transactormap-buffer-merge! buffer-am)
       result)
      (#('fail exception)
       ;; Decorate exception with the vat event context.
       (let ((vat-error (make-exception (make-vat-turn-error event)
                                        exception)))
         (vat-log-error! vat event vat-error)
         `#(fail ,vat-error)))))
  (define (make-turn-snapshot)
    (define snapshot* (copy-whactormap snapshot))
    (define transactormap (transactormap-reparent new-am snapshot*))
    (transactormap-merge! transactormap)
    snapshot*)
  (define (churn)
    (unless (q-empty? near-q)
      (match (deq! near-q)
        ((prev-event msg)
         (let ((churn-id (vat-current-churn vat))
               (event (make-vat-event 'receive churn-id
                                      (vat-next-timestamp vat)
                                      #f msg (make-turn-snapshot))))
           (vat-log-append! vat event prev-event)
           (turn event)
           ;; Continue processing the near messages.
           (churn))))))
  ;; Take an initial turn.
  (define received-at (vat-next-timestamp vat sent-at))
  (define init-event
    (make-vat-event 'receive churn-id received-at sent-at msg snapshot))
  (vat-log-append! vat init-event #f)
  (define result (turn init-event))
  ;; Turn as many additional times as it takes to run this vat to
  ;; quiescence.
  (churn)
  ;; Dispatch far messages.
  (let loop ()
    (unless (q-empty? far-q)
      (match (deq! far-q)
        ((prev-event far-msg)
         (let* ((time (vat-next-timestamp vat))
                (event (make-vat-event 'send churn-id time #f far-msg snapshot)))
           (vat-log-append! vat event prev-event)
           (dispatch-message far-msg time)
           (loop))))))
  ;; And now let's return everything...
  (values result new-am))

(define (vat-start! vat)
  "Start processing turns for VAT."
  (define running? (vat-running vat))
  (define actormap (vat-actormap vat))
  (define (maybe-merge returned am)
    (match returned
      [#('ok rval)
       (transactormap-merge! am)]
      [_ #f]))
  (define (call-with-error-handling thunk handler)
    (call/ec
     (lambda (abort)
       (define (handle-error exn)
         (define stack (make-stack #t handle-error))
         (display-backtrace stack (current-error-port))
         (newline (current-error-port))
         (abort (handler exn)))
       (with-exception-handler handle-error thunk))))
  (define (churn envelope)
    (call-with-error-handling
     (lambda ()
       (let* ((msg (vat-envelope-message envelope))
              (sent-at (vat-envelope-timestamp envelope)))
         (define-values (returned new-actormap)
           (vat-churn vat msg sent-at))
         (maybe-merge returned new-actormap)
         returned))
     (lambda (exn)
       `#(fail ,exn))))
  (unless (atomic-box-ref running?)
    (atomic-box-set! running? #t)
    ((vat-start-proc vat) churn)))

(define (vat-send vat envelope)
  ((vat-send-proc vat) envelope))

;; This simple actor constructor is used to give a descriptive name to
;; the one-off actors created by call-with-vat so that they are
;; clearly marked when debugging.
(define (^call-with-vat _bcom thunk)
  thunk)

(define (call-with-vat vat thunk)
  "Run THUNK in the context of VAT and return the resulting values."
  (if (vat-running? vat)
      (let ((am (vat-actormap vat)))
        ;; The user provided thunk is going to be called
        ;; asynchronously within a vat turn, likely in another thread,
        ;; which makes handling multiple return values tricky.  To
        ;; make things easy for vat implementations, we wrap up all of
        ;; the original thunk's return values into a list so there's
        ;; only a single value to pass back.  Here in the caller's
        ;; thread, the list gets converted back into multiple return
        ;; values.
        (define (multi-value-thunk)
          (call-with-values thunk list))
        ;; Spawn a throwaway actor whose behavior is just to apply the
        ;; thunk.
        (define refr (actormap-spawn! am ^call-with-vat multi-value-thunk))
        (define msg (make-message (vat-connector vat) refr #f '()))
        (match (vat-send vat (make-vat-envelope msg 0 #t))
          (#('ok vals) (apply values vals))
          (#('fail err) (raise-exception err))))
      (error "vat is not running" vat)))

(define-syntax-rule (with-vat vat body ...)
  (call-with-vat vat (lambda () body ...)))

(define (vat-logging? vat)
  "Return #t if event logging is enabled for VAT."
  (atomic-box-ref (%vat-logging? vat)))

(define (set-vat-logging! vat log?)
  "If LOG? is #t, enable event logging for VAT.  Otherwise, disable
logging."
  (atomic-box-set! (%vat-logging? vat) log?))

(define (vat-log-capacity vat)
  "Return the maximum number of events that VAT can keep in its log."
  (%vat-log-capacity (vat-log vat)))

(define (vat-log-length vat)
  "Return the length of the event for VAT."
  (%vat-log-length (vat-log vat)))

(define (vat-log-ref vat i)
  "Return the event at index I in VAT."
  (%vat-log-ref (vat-log vat) i))

(define (vat-log-ref-by-time vat t)
  "Return the event at timestamp T in VAT."
  (%vat-log-ref-by-time (vat-log vat) t))

(define (vat-log-ref-by-message vat msg)
  "Return the event associated with MSG in VAT."
  (%vat-log-ref-by-message (vat-log vat) msg))

(define (vat-log-ref-previous vat event)
  "Return the event that caused EVENT in VAT, if any."
  (%vat-log-ref-previous (vat-log vat) event))

(define (vat-log-ref-next vat event)
  "Return the event caused by EVENT in VAT, if any."
  (%vat-log-ref-next (vat-log vat) event))

(define (vat-log-append! vat event prev)
  (when (vat-logging? vat)
    (%vat-log-append! (vat-log vat) event prev)))

(define (vat-log-error! vat event exception)
  (when (vat-logging? vat)
    (%vat-log-error! (vat-log vat) event exception)))

(define (vat-log-resize! vat capacity)
  "Resize the event log of VAT to CAPACITY."
  (%vat-log-resize! (vat-log vat) capacity))

(define (vat-log-clear! vat)
  "Delete all logged events from VAT."
  (%vat-log-clear! (vat-log vat)))

(define (vat-log-error-for-event vat event)
  "Return the error associated with EVENT in VAT, if any."
  (%vat-log-error-for-event (vat-log vat) event))

(define (vat-log-errors vat)
  "Return all known errors that have occurred in VAT."
  (%vat-log-errors (vat-log vat)))

(define* (make-fibrous-vat #:key name log?
                           (log-capacity default-log-capacity)
                           (scheduler (default-vat-scheduler))
                           (dynamic-wrap port-redirect-dynamic-wrap))
  (define done? (make-condition))
  (define-values (enq-ch deq-ch stop?)
    (spawn-delivery-agent #:scheduler scheduler))
  (define (start churn)
    (define (handle-message args)
      (match args
        ((envelope return-ch)
         ;; We have the put-message be run in its own fiber so that if
         ;; the other side isn't listening for it anymore, the vat
         ;; itself doesn't end up blocked.
         (syscaller-free-fiber
          (lambda ()
            (put-message return-ch (churn envelope)))))
        (envelope
         (churn envelope))))
    (define (loop)
      ;; This loop will repeatedly handle a new message or detect if
      ;; the 'done?' condition has been signalled.  The message
      ;; handler will churn the vat and loop.  The loop terminates
      ;; when the 'done?' condition is signalled.
      (and (perform-operation
            (choice-operation (wrap-operation (get-operation deq-ch)
                                              (lambda (args)
                                                (handle-message args)
                                                #t))
                              (wrap-operation (wait-operation done?)
                                              (lambda () #f))))
           (loop)))
    ;; So much nesting you might think a bird wrote this.
    (call-with-new-thread
     (lambda ()
       (run-fibers
        (lambda ()
          (dynamic-wrap
           (lambda ()
             (syscaller-free
              (lambda ()
                (spawn-fiber loop scheduler)
                (wait done?))))))))))
  (define (halt)
    (signal-condition! done?)
    *unspecified*)
  (define (send envelope)
    (if (vat-envelope-return? envelope)
        (let ((return-ch (make-channel)))
          (put-message enq-ch (list envelope return-ch))
          (get-message return-ch))
        (put-message enq-ch envelope)))
  (make-vat #:name name
            #:log? log?
            #:log-capacity log-capacity
            #:start start
            #:halt halt
            #:send send))

(define* (spawn-fibrous-vat #:key name log?
                            (log-capacity default-log-capacity)
                            (scheduler (default-vat-scheduler))
                            (dynamic-wrap port-redirect-dynamic-wrap))
  (let ((vat (make-fibrous-vat #:name name
                               #:log? log?
                               #:log-capacity log-capacity
                               #:scheduler scheduler
                               #:dynamic-wrap dynamic-wrap)))
    (vat-start! vat)
    vat))

(define* (spawn-vat #:key name log? (log-capacity default-log-capacity))
  (spawn-fibrous-vat #:name name
                     #:log? log?
                     #:log-capacity log-capacity))

(define (syscaller-free-fiber thunk)
  (syscaller-free
   (lambda ()
     (spawn-fiber thunk))))

(define (spawn-fibrous-vow proc)
  (define-values (promise resolver)
    (spawn-promise-values))
  (syscaller-free-fiber
   (lambda ()
     (call/ec
      (lambda (abort)
        (define (handle-exn exn)
          (define stack
            (make-stack #t handle-exn))
          (display "Error in spawn-fibrous-vow:\n" (current-error-port))
          (format (current-error-port) "~a\n" exn)
          ((@@ (goblins core) display-backtrace*) stack)
          (<-np-extern resolver 'break exn)
          (abort))
        (define (run-and-send)
          ;; TODO: Add error handling
          (define result (proc))
          (<-np-extern resolver 'fulfill result))
        (with-exception-handler handle-exn
          run-and-send)))))
  promise)

(define-syntax-rule (fibrous body ...)
  (spawn-fibrous-vow (lambda () body ...)))

(define-syntax define-vat-run
  (syntax-rules ()
    ((define-vat-run vat-run-id vat)
     (begin
       (define this-vat vat)
       (define-syntax vat-run-id
         (syntax-rules ::: ()
                       ((_ body :::)
                        (with-vat this-vat body :::))))))
    ((define-vat-run vat-run-id)
     (define-vat-run vat-run-id (spawn-vat)))))

;; An example to test against, wip
#;(run-fibers
 (lambda ()
   (define a-vat (spawn-vat))
   (a-vat 'run
          (lambda ()
            (define peeker
              (spawn (lambda _ (lambda (msg) (pk 'msg msg)))))
            (define (^sleppy _bcom my-name)
              (lambda (sleep-for)
                (pk 'sleepin my-name)
                (await (fibrous (sleep sleep-for)
                                'done))
                (pk 'im-up-im-up my-name)))
            (define sleppy-sam
              (spawn ^sleppy 'sam))
            (define sleppy-sarah
              (spawn ^sleppy 'sarah))
            (<-np peeker 'hi)
            (<-np peeker 'there)
            (<-np sleppy-sam 1)
            (<-np sleppy-sarah .5))))
 #:drain? #t)
