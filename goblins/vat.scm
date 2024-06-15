;;; Copyright 2021-2022 Christine Lemmer-Webber
;;; Copyright 2022-2024 Jessica Tallon
;;; Copyright 2023 David Thompson
;;; Copyright 2023 Juliana Sims
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
  #:use-module (goblins core-types)
  #:use-module (goblins ghash)
  #:use-module (goblins inbox)
  #:use-module (goblins abstract-types)
  #:use-module (goblins default-vat-scheduler)
  #:use-module (goblins utils random-name)
  #:use-module (goblins utils ring-buffer)
  #:use-module (gcrypt random)
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)      ; lists
  #:use-module (srfi srfi-9)      ; records
  #:use-module (srfi srfi-9 gnu)  ; record extensions
  #:use-module (srfi srfi-11)     ; let-values
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
            vat-event-tree-map
            vat-event-tree-filter
            vat-event-tree-remove
            vat-event-tree->timeline

            &vat-turn-error
            vat-turn-error-event

            vat-envelope?
            vat-envelope-message
            vat-envelope-timestamp
            vat-envelope-return?

            make-vat-system-operation
            vat-system-operation?
            vat-system-operation-proc

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

            call-system-op-with-vat

            syscaller-free-fiber
            spawn-fibrous-vow
            fibrous

            spawn-persistent-vat
            vat-take-portrait!
            vat-replace-behavior!
            vat-take-single-object-portrait

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
;;; or CapTP connector (depending on if local/remote).
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

;; This is a container for holding onto everything a vat needs to
;; persist. This could live just on the vat itself but since it's a
;; lot of stuff, it's broken into its own record.
(define-record-type <vat-persistence>
  (make-vat-persistence vat-aurie-id persistence-environ persist-on
                        store read-portrait! val->slot-ref roots)
  vat-persistence-env?
  ;; A permanent ID which other vats can use to reference a given vat.
  (vat-aurie-id vat-persistence-vat-aurie-id)
  ;; This is a <persistence-env> with all objects in the graph.
  (persistence-environ vat-persistence-environ set-vat-persistence-environ!)
  ;; When 'churn it tells the vat to persist on churns, otherwise
  ;; manual persist manually with `vat-take-portrait!'
  (persist-on vat-persistence-persist-on)
  ;; The store we should persist two and restore from.
  (store vat-persistence-store)
  ;; This is a function we get from core.scm to persist a single object.
  (read-portrait! vat-persistence-read-portrait!
                    set-vat-persistence-read-portrait!)
  ;; This is a function we get from core.scm to lookup the slot for a refr.
  (val->slot-ref vat-persistence-val->slot-ref
                 set-vat-persistence-val->ref!)
  ;; The root objects in the graph.
  (roots vat-persistence-roots set-vat-persistence-roots!))

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
  (snapshot vat-event-snapshot set-vat-event-snapshot!))

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

(define (refr-vat-connector refr)
  (if (local-refr? refr)
      (local-refr-vat-connector refr)
      ;; HACK: Return a fake remote vat connector.  We are not
      ;; currently able to inspect remote vat events but we
      ;; don't want the debugging tools to throw an error.
      (match-lambda*
        (('name) "Remote")
        (('find-event-by-time _) #f)
        (('find-event-by-message _) #f)
        (('find-previous-event _) #f)
        (('find-next-events _) '()))))

(define (vat-event-connector event)
  "Return the connector for the vat that EVENT belongs to. Send events
belong to the sender.  Receive events belong to the receiver."
  (let ((type (vat-event-type event))
        (msg (vat-event-message event)))
    (if (eq? type 'send)
        (message-or-request-from-vat msg)
        (refr-vat-connector (message-or-request-to msg)))))

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
               (vat-connector (refr-vat-connector
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

(define (vat-event-tree-map proc tree)
  "Recursively apply PROC to all leaf nodes and subtrees of TREE, a tree
of vat events in the format produced by 'vat-event-tree', and return a
new tree.  Post-order tree traversal is used so that PROC is applied
to leaf nodes before their parent trees."
  (match tree
    ((root children ...)
     (proc (cons root
                 (map (lambda (child)
                        (vat-event-tree-map proc child))
                      children))))
    ((? vat-event? leaf)
     (proc leaf))))

(define (vat-event-tree-filter pred tree)
  "Recursively apply PRED to all leaf nodes and subtrees of TREE, a tree
of vat events in the format produced by 'vat-event-tree', and return a
new tree consisting of the nodes for which PRED returns #t.
Post-order tree traversal is used so that PRED is applied to leaf
nodes before their parent trees."
  (match tree
    (((? vat-event? root) children ..1)
     (match (filter-map (lambda (child)
                          (vat-event-tree-filter pred child))
                        children)
       (() root)
       ((children* ...)
        (let ((filtered (cons root children*)))
          (and (pred filtered) filtered)))))
    ((? vat-event? leaf)
     (and (pred leaf) leaf))))

(define (vat-event-tree-remove pred tree)
  "Like 'vat-event-tree-filter', but nodes of TREE that match PRED are
removed."
  (vat-event-tree-filter (negate pred) tree))

(define (vat-event-tree->timeline tree)
  "Convert TREE, a tree of vat events as produced by 'vat-event-tree',
to vat timeline form.  A vat timeline is a graph structure consisting
of a hash table mapping vat connectors to their respective events in
TREE.  The events within a timeline are also stored in hash tables,
mapping timestamps to lists whose head is a local vat event and all
subsequent elements are far events that were caused by the local
event.

Retrieving an event list for a given vat connector and timestamp looks
like this:

(hashv-ref (hashq-ref timeline vat-connector) timestamp)"
  (let ((timeline (make-hash-table)))
    (define (events-for-vat vat-connector)
      (or (hashq-ref timeline vat-connector)
          (let ((table (make-hash-table)))
            (hashq-set! timeline vat-connector table)
            table)))
    (define (add-to-timeline vat-connector timestamp events)
      (hashq-set! (events-for-vat vat-connector) timestamp events))
    (define (loop tree)
      (match tree
        ((root children ...)
         (let* ((vat-connector (vat-event-connector root))
                ;; Build a list of *outgoing* send events with each
                ;; element of the form (vat-connector timestamp).
                (sends (filter-map (match-lambda
                                     ((or (child _ ...) child) ; match subtree or leaf node
                                      (let ((other-connector (vat-event-connector child)))
                                        ;; Filter out child events that
                                        ;; are from the same vat.  We only
                                        ;; care about events in other vats
                                        ;; here.
                                        (and (not (eq? vat-connector other-connector))
                                             (list other-connector (vat-event-timestamp child))))))
                                   children)))
           ;; Add root event to the timeline.
           (add-to-timeline vat-connector
                            (vat-event-timestamp root)
                            (cons root sends))
           ;; Recursively add child events to the timeline.
           (for-each loop children)))
        ;; Base case: A leaf node.
        ((? vat-event? event)
         (add-to-timeline (vat-event-connector event)
                          (vat-event-timestamp event)
                          (list event)))))
    (loop tree)
    timeline))

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

(define (print-vat-log log port)
  (format port
          "#<vat-log length: ~a time-index: ~a message-index: ~a prev-index: ~a next-index: ~a error-index: ~a>"
          (ring-buffer-length (vat-log-events log))
          (vat-log-time-index log)
          (vat-log-message-index log)
          (vat-log-prev-index log)
          (vat-log-next-index log)
          (vat-log-error-index log)))

(set-record-type-printer! <vat-log> print-vat-log)

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
      ;; Only add to the prev/next indexes if there is a previous
      ;; event.  This is particularly important for the next index,
      ;; because otherwise every root event would be consed onto a
      ;; list of events associated with the key #f.  This list would
      ;; grow without bound, eventually exhausting all memory.
      (when prev
        (hashq-set! prev-index event prev)
        (hashq-set! next-index prev
                    (cons event (hashq-ref next-index prev '())))))))

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


;; Inter-vat Aurie vat registry and retrievers
;; ===========================================

;; This section is for inter-vat Aurie integration.
;; DO NOT EXPORT ANY OF THIS MACHINERY FROM THIS MODULE!
;;
;; Inter-vat Aurie works by:
;;  - vats having randomly generated identifiers made *specifically*
;;    for being identified for Aurie integerchange.  There's not meant
;;    to be any way to make them yourself; a persistent vat booting up
;;    will automatically make its own id, register with the given
;;    registry, and upon being restored, reawake with that id.
;;  - local-object-refrs also have unique, incremented integer ids
;;    specific to that vat (or really, actormap).  These also are not
;;    meant to be used for anything other than inter-vat Aurie.

;; Two request types for aurie registries follow.
;; We want to protect these but allow them to be generally available
;; for all vats to use, and unexported records are reasonable ways of
;; performing rights amplification.

;; Request to register a vat
(define-record-type <register-request>
  (make-register-request vat-aurie-id vat)
  register-request?
  (vat-aurie-id register-request-vat-aurie-id)
  (vat register-request-vat))

(define-record-type <registry-fetch-vat>
  (make-registry-fetch-vat vat-aurie-id)
  registry-fetch-vat?
  (vat-aurie-id registry-fetch-vat-vat-aurie-id))

(define* (^aurie-registry bcom #:optional (vat-id->vat ghash-null))
  (match-lambda
    ((? register-request? reg-request)
     (define vat-aurie-id
       (register-request-vat-aurie-id reg-request))
     (define vat-to-register
       (register-request-vat reg-request))
     ;; fulfill a waiting resolver, if there is one
     (match (ghash-ref vat-id->vat vat-aurie-id #f)
       (('waiting _registered-vat-vow registered-vat-resolver)
        ($ registered-vat-resolver 'fulfill vat-to-register))
       (_ 'no-op))
     ;; but regardless, become a new version of the registry with the
     ;; registered-vat being set
     (bcom (^aurie-registry
            bcom (ghash-set vat-id->vat vat-aurie-id vat-to-register))))
    ((? registry-fetch-vat? reg-fetch-req)
     (define vat-aurie-id
       (registry-fetch-vat-vat-aurie-id reg-fetch-req))
     (match (ghash-ref vat-id->vat vat-aurie-id #f)
       ;; There's a version waiting
       (('waiting registered-vat-vow _registered-vat-resolver)
        registered-vat-vow)
       ;; Nothing is waiting, but we also don't have a resolution, so
       ;; let's add a waiting request
       (#f
        (let*-values (((registered-vat-vow registered-vat-resolver)
                       (spawn-promise-values))
                      ((new-vat-id->vat)
                       (ghash-set vat-id->vat
                                  vat-aurie-id
                                  (list 'waiting registered-vat-vow
                                        registered-vat-resolver))))
          (bcom (^aurie-registry bcom new-vat-id->vat)
                registered-vat-vow)))
       ;; otherwise, it must be the registered vat, so return that
       (vat vat)))))

;; This is not optimized for efficiency, retrieval of objects by Aurie
;; id is only meant to be done at startup, so the entire actormap is
;; traversed to determine the relevant object refrs matching
;; identifiers.

(define (vat-resolve-objs-by-aurie-ids vat ids-and-resolvers)
  (call-system-op-with-vat 
   vat
   (lambda (vat)
     (define am (vat-actormap vat))
     (define mapping (make-hash-table))
     (match am
       ;; TODO: Only whactormaps supported so far.  We should really
       ;; support actormap-for-each / actormap-fold...
       ((? whactormap?)
        (let ((wht (whactormap-data-wht (actormap-data am))))
          (hash-for-each (lambda (obj-refr _v)
                           (hashv-set! mapping 
                                       (local-object-refr-aurie-id obj-refr)
                                       obj-refr))
                         wht))))
     (for-each (match-lambda
                 ((id . resolver)
                  (let ((obj-with-id (hashv-ref mapping id #f)))
                    (if obj-with-id
                        (<-np-extern resolver 'fulfill obj-with-id)
                        (<-np-extern resolver 'break
                                     'no-such-object)))))
               ids-and-resolvers))))


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

(define-record-type <vat-system-operation>
  (make-vat-system-operation proc)
  vat-system-operation?
  (proc vat-system-operation-proc))

(define-record-type <vat>
  (%make-vat id name actormap running connector current-churn
             clock logging? log start-proc halt-proc send-proc
             persistence-env)
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
  (send-proc vat-send-proc)
  (persistence-env vat-persistence-env))

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

(define* (make-vat #:key persistence-env name start halt send log?
                   (log-capacity default-log-capacity))
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
disabled.  LOG-CAPACITY events will be retained in the log.

Type: (Optional (#:name (U String Symbol)))
(Optional (#:start (Message -> Void))) (Optional (#:halt (-> Void)))
(Optional (#:send (Message Boolean -> (U Void Any))))
(Optional (#:log? Boolean))
(Optional (#:log-capacity Positive-Number)) -> Void"
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
       (vat-log-ref-next vat event))
      (('aurie-vat-id)
       (let ((persistence-env (vat-persistence-env vat)))
         (if persistence-env
             (vat-persistence-vat-aurie-id persistence-env)
             #f)))))
  (define am (make-actormap #:vat-connector connector))
  (define id (next-vat-id))
  (define clock (make-atomic-box 0))
  (define running? (make-atomic-box #f))
  (define logging? (make-atomic-box log?))
  (define log (make-vat-log log-capacity))
  (define index (make-hash-table))
  (define vat
    (%make-vat id name am running? connector 0 clock logging? log
               start halt send persistence-env))
  (register-vat! vat)
  vat)

(define (vat-running? vat)
  "Return #t if VAT is currently running, else #f.

Type: Vat -> Boolean"
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
  "Stop processing turns for VAT.

Type: Vat -> Void"
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
  (define* (current-snapshot)
    (define snapshot* (copy-whactormap snapshot))
    (define transactormap (transactormap-reparent new-am snapshot*))
    (transactormap-merge! transactormap)
    snapshot*)
  (define (queue-messages-appropriately! prev-event msgs)
    (match msgs
      (() 'done)
      ((msg next-msgs ...)
       ;; Last message first.
       (queue-messages-appropriately! prev-event next-msgs)
       ;; Create new event and put it in either the near or far queue.
       (let* ((near? (near-msg? msg))
              (event-type (if near? 'receive 'send))
              (timestamp (vat-next-timestamp vat))
              ;; For near messages, the snapshot will be set during
              ;; its turn.  Far messages are not processed in the
              ;; current vat, so we use the current snapshot so users
              ;; can inspect the state of the actormap when the far
              ;; message was sent.
              (snapshot (if near? #f (current-snapshot)))
              (q (if near? near-q far-q)))
         (let ((event (make-vat-event event-type churn-id timestamp
                                      #f msg snapshot)))
           (vat-log-append! vat event prev-event)
           (enq! q event))))))
  (define (turn event)
    (set-vat-event-snapshot! event (current-snapshot))
    (define msg (vat-event-message event))
    (define-values (result buffer-am new-msgs)
      (actormap-turn-message new-am msg #:catch-errors? #t))
    (define result*
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
    ;; Queue messages after merging 'buffer-am' so we can take a
    ;; snapshot to associate with far message events.
    (queue-messages-appropriately! event new-msgs)
    result*)
  (define (churn)
    (unless (q-empty? near-q)
      (turn (deq! near-q))
      ;; Continue processing the near messages.
      (churn)))
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
      (let ((event (deq! far-q)))
        (dispatch-message (vat-event-message event)
                          (vat-event-timestamp event))
        (loop))))

  ;; Persist the changes if needed.
  (vat-maybe-persist-changed-objs! vat new-am)

  ;; And now let's return everything...
  (values result new-am))

(define (vat-start! vat)
  "Start processing turns for VAT.

Type: Vat -> Void"
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
  (define (churn-message msg sent-at)
    (define-values (returned new-actormap)
      (vat-churn vat msg sent-at))
    (maybe-merge returned new-actormap)
    returned)
  (define (churn envelope)
    (call-with-error-handling
     (lambda ()
       (let* ((msg-or-proc (vat-envelope-message envelope))
              (sent-at (vat-envelope-timestamp envelope)))
         (match msg-or-proc
           ;; Special case, we're asking to "run" a specific
           ;; procedure for call-with-vat
           ((? procedure? thunk)
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
            (define refr (actormap-spawn! actormap ^call-with-vat
                                          multi-value-thunk))
            (define msg
              (make-message (vat-connector vat) refr #f '()))
            (churn-message msg sent-at))
           ;; A "system" operation is a special kind which permits operating on
           ;; the vat's low-level actormap and possibly other features directly.
           ((? vat-system-operation? system-op)
            (define system-op-proc
              (vat-system-operation-proc system-op))
            (system-op-proc vat))
           ;; The usual case.
           (msg
            (churn-message msg sent-at)))))
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
  "Run THUNK in the context of VAT and return the resulting values.

Type: Vat (-> Any) -> Any"
  (if (vat-running? vat)
      (match (vat-send vat (make-vat-envelope thunk 0 #t))
        (#('ok '*awaited*) '*awaited*)
        (#('ok vals) (apply values vals))
        (#('fail err) (raise-exception err)))
      (error "vat is not running" vat)))

(define (call-system-op-with-vat vat system-op-proc)
  (define envelope
    (make-vat-envelope (make-vat-system-operation system-op-proc)
                       0 #t))
  (vat-send vat envelope))

(define-syntax-rule (with-vat vat body ...)
  ;;; Evaluate BODY in the context of VAT and return resulting values.
  ;;;
  ;;; Type: Vat Expression ... -> Any
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
                           (persistence-env #f)
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
         (define result (churn envelope))
         ;; We have the put-message be run in its own fiber so that if
         ;; the other side isn't listening for it anymore, the vat
         ;; itself doesn't end up blocked.
         (syscaller-free-fiber
          (lambda ()
            (put-message return-ch result))))
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
    (dynamic-wrap
     (lambda ()
       (syscaller-free
        (lambda ()
          (spawn-fiber loop scheduler))))))
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
            #:send send
            #:persistence-env persistence-env))

(define* (spawn-fibrous-vat #:key name log?
                            (persistence-env #f)
                            (log-capacity default-log-capacity)
                            (scheduler (default-vat-scheduler))
                            (dynamic-wrap port-redirect-dynamic-wrap))
  (let ((vat (make-fibrous-vat #:name name
                               #:log? log?
                               #:log-capacity log-capacity
                               #:scheduler scheduler
                               #:dynamic-wrap dynamic-wrap
                               #:persistence-env persistence-env)))
    (vat-start! vat)
    vat))


(define* (spawn-vat #:key name log? (log-capacity default-log-capacity))
  "Create and return a reference to a new vat. If provided, NAME is
the debug name of the vat. If LOG? is #t, log vat events, otherwise
do not. If provided, LOG-CAPACITY is the number of events to retain in
the log.

Type: (Optional (#:name (U String Symbol)) (Optional (#:log? Boolean))
(Optional (#:log-capacity Positive-Number)) -> Vat"
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
       (issue-deprecation-warning
        "`define-vat-run' is deprecated. Use `call-with-vat', `with-vat', or `,enter-vat' REPL command instead.")
       (define this-vat vat)
       (define-syntax vat-run-id
         (syntax-rules ::: ()
                       ((_ body :::)
                        (with-vat this-vat body :::))))))
    ((define-vat-run vat-run-id)
     (define-vat-run vat-run-id (spawn-vat)))))


;; Vat persistence
;; ===============


(define (transactormap-calculate-obj-delta am)
  "Gets the refrs of all objects that changed in last transaction"
  (define actormap-data
    (@@ (goblins core) actormap-data))
  (define transactormap-data-delta
    (@@ (goblins core) transactormap-data-delta))

  (define am-data
    (actormap-data am))
  (define delta-obj-map
    (transactormap-data-delta am-data))
  ;; Extract just the refr.
  (hash-fold
   (lambda (refr mactor prev)
     (cons refr prev))
   '()
   delta-obj-map))

(define (vat-take-portrait!* vat)
  (define persistence-env
    (vat-persistence-env vat))
  (unless (and persistence-env
               (vat-persistence-read-portrait! persistence-env)
               (vat-persistence-val->slot-ref persistence-env)
               (vat-persistence-roots persistence-env))
    (error "Not a persistence aware vat"))

  ;; Setup everything we need to take a full portrait
  (define am
    (vat-actormap vat))
  (define read-portrait!
    (vat-persistence-read-portrait! persistence-env))
  (define val->slot-refr
    (vat-persistence-val->slot-ref persistence-env))
  (define roots
    (vat-persistence-roots persistence-env))
  (define-values (slot->portrait root-slots)
    (actormap-take-portrait-with-read-portrait am read-portrait! val->slot-refr roots))

  ;; Now save the portrait and roots in the store
  (define store
    (vat-persistence-store persistence-env))
  (define save-portrait-in-store!
    (persistence-store-save-proc store))
  (define aurie-vat-id
    (vat-persistence-vat-aurie-id persistence-env))
  (save-portrait-in-store! 'save-graph aurie-vat-id slot->portrait root-slots))

(define (vat-take-portrait! vat)
  (call-system-op-with-vat vat vat-take-portrait!*))

(define* (spawn-persistent-vat persistence-env spawn-roots-thunk store
                               #:key (persist-on 'churn)
                               (vat-constructor spawn-fibrous-vat)
                               name log? (log-capacity default-log-capacity)
                               aurie-registry)
  "Create and return a reference to a new vat with persistence. All
objects spawned on the vat that will persist must be persistence
aware. The objects must be in PERSISTENCE-ENV which is used when the
vat takes the portrait and rehydrates objects.

The SPAWN-ROOT-THUNK perameter will be run within the vat
environment and should spawn one or more values which are the root
objects to be persisted.

STORE is a storage backend mechanism which matches the persistence
store interface.

If PERSIST-ON is not provided persistence will happen on every churn
of the vat. If this is #f, no automatic persistence mechanism
will occur and this should be handled manually.

If provided, NAME is the debug name of the vat. If LOG? is #t, log
vat events, otherwise do not. If provided, LOG-CAPACITY is the number
of events to retain in the log.

TODO: Document AURIE-REGISTRY
"
  ;; We should either restore from the data in the store if that exists,
  ;; or we should spawn the roots by using `spawn-roots-lambda'.
  (define read-from-store
    (persistence-store-read-proc store))
  (define-values (vat-aurie-id portraits root-slots)
    (read-from-store 'graph-and-slots))

  (define current-vat-aurie-id
    (if vat-aurie-id
        vat-aurie-id
        (gen-random-bv 32 %gcry-strong-random)))

  (define vat-persistence
    (make-vat-persistence current-vat-aurie-id persistence-env
                          persist-on store #f #f #f))
  (define vat
    (vat-constructor
     #:persistence-env vat-persistence
     #:name name
     #:log? log?
     #:log-capacity log-capacity))

  ;; TODO: we'll also want to gather up vats-we-found-ids-in here
  (define roots
    (if (and portraits root-slots)
        (call-system-op-with-vat
         vat (lambda (vat)
               (define vat-am
                 (vat-actormap vat))
               (call-with-values
                   (lambda ()
                     (actormap-restore! vat-am persistence-env portraits root-slots))
                 list)))
        (with-vat vat
          (call-with-values spawn-roots-thunk list))))

  (define-values (read-portrait! val->slot-ref)
    (make-actormap-read-portrait! persistence-env roots))

  (call-system-op-with-vat
   vat (lambda (vat)
         ;; Setup the persistent environment
         (set-vat-persistence-read-portrait! vat-persistence read-portrait!)
         (set-vat-persistence-val->ref! vat-persistence val->slot-ref)
         (set-vat-persistence-roots! vat-persistence roots)

         ;; Finally, lets take the first vat portrait
         (vat-take-portrait!* vat)))

  ;; TODO: If there's no aurie registry should we break all the
  ;; promises requested immediately?
  #;(when aurie-registry
    ;; Register this vat.
    ;;
    ;; We wait to talk to the registry until after all our Aurie objects
    ;; are restored to avoid race conditions.
    (<-np-extern aurie-registry
                 (make-register-request current-vat-aurie-id vat))

    ;; TODO: RESUME HERE after gathering all objects to be resolved
    ;; using `vat-resolve-objs-by-aurie-ids'
    (for-each (match-lambda
                ;; Iterating over pairs of aurie-vat-ids and the object aurie-ids
                ;; we want to retrieve
                ((far-vat-aurie-id . ids-and-resolvers)
                 (call-with-vat
                  vat
                  (lambda ()
                    (on (<- aurie-registry (make-registry-fetch-vat far-vat-aurie-id))
                        (lambda (far-vat-for-aurie-interlink)
                          (syscaller-free-fiber
                           (lambda ()
                             (vat-resolve-objs-by-aurie-ids vat ids-and-resolvers)))))))))
              vats-we-found-ids-in))

  (apply values vat roots))

(define (vat-maybe-persist-changed-objs! vat new-am)
  (define vat-persistence
    (vat-persistence-env vat))

  (when vat-persistence
    (let* ([process-queue (make-q)]
           [slot->portraits (make-hash-table)]
           [persist-on (vat-persistence-persist-on vat-persistence)]
           [val->slot-refr (vat-persistence-val->slot-ref vat-persistence)]
           [read-portrait! (vat-persistence-read-portrait! vat-persistence)]
           [store (vat-persistence-store vat-persistence)]
           [save-portraits! (persistence-store-save-proc store)])

      ;; On the first churn when a persistent vat is setting up these are not available
      ;; despite the vat being setup for persistence. In such a case skip this.
      (when (and (eq? persist-on 'churn) val->slot-refr)
        ;; From the set of changed objects in the last transaction, find the ones which
        ;; appear in the portrait of the object graph by checking if they have an
        ;; assigned slot. For the ones found queue them up for depiction
        (for-each
         (lambda (changed-obj)
           (when (val->slot-refr changed-obj)
             (enq! process-queue changed-obj)))
         (transactormap-calculate-obj-delta new-am))

        (while (not (q-empty? process-queue))
          (let ((obj (deq! process-queue)))
            (define-values (slot portrait new-child-objs)
              (read-portrait! new-am obj))

            (hashq-set! slot->portraits slot portrait)

            ;; The object may have changed by adding a new object not previously in the
            ;; object graph. In such cases we need to ensure they're queued also.
            (hash-for-each
             (lambda (obj _val)
               (unless (memq obj (car process-queue))
                 (enq! process-queue obj)))
             new-child-objs)))

        (save-portraits! 'save-delta slot->portraits)))))

(define (vat-take-single-object-portrait vat refr)
  (define (take-object-portrait vat)
    (define persistence-env
      (vat-persistence-env vat))
    (unless persistence-env
      (error "Cannot get portrait in a non-persistent capable vat" vat))

    (define environ
      (vat-persistence-environ persistence-env))
    (define am
      (vat-actormap vat))

    ;; Because we are not committing this, we don't want to use
    ;; the standard "read-portrait" functions we normally would
    ;; we should get the self-portrait function and just give
    ;; that data.
    (define get-self-portrait
      (@@ (goblins core) mactor:object-self-portrait))
    (define mactor
      (actormap-ref am refr))

    (unless mactor
      (error "refr not found in vat" refr))
    (define take-self-portrait
      (get-self-portrait mactor))
    (take-self-portrait))
  (call-system-op-with-vat vat take-object-portrait))

(define* (vat-replace-behavior! vat #:optional new-env)
  (define (replace-behavior! vat)
    (define vat-persistence
      (vat-persistence-env vat))
    (unless vat-persistence
      (error "Cannot replace the behavior on a non-persistent vat"))

    (when new-env
      (set-vat-persistence-environ! vat-persistence new-env))

    (define am
      (vat-actormap vat))
    ;; Actually perform the upgrade
    (actormap-replace-behavior!
     am
     (vat-persistence-environ vat-persistence)))
  (call-system-op-with-vat vat replace-behavior!))

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

