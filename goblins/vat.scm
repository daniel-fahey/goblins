;;; Copyright 2021-2022 Christine Lemmer-Webber
;;; Copyright 2022 Jessica Tallon
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
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-vat
            vat?
            vat-name
            vat-running?
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

(define (generate-random-vat-name)
  (random-name 8))

(define-record-type <vat>
  (%make-vat name actormap running start-proc halt-proc send-proc)
  vat?
  (name vat-name)
  (actormap vat-actormap)
  (running vat-running)
  (start-proc vat-start-proc)
  (halt-proc vat-halt-proc)
  (send-proc vat-send-proc))

(define (print-vat vat port)
  (format port "#<vat ~a>" (vat-name vat)))

(set-record-type-printer! <vat> print-vat)

(define* (make-vat #:key (name (generate-random-vat-name))
                   start halt send)
  "Return a new vat named NAME.  Vat behavior is determined by three
event hooks:

START: A procedure that starts the vat process, presumably in a new
thread or other non-blocking manner. Accepts one argument: a procedure
which takes a message as its one argument and churns the underlying
actormap for the vat.

HALT: A thunk that stops the vat process.

SEND: A procedure that accepts a message to handle within the vat
process and a boolean flag indicating if the message result needs to
be returned to the sender or not."
  (define running? (make-atomic-box #f))
  (define (vat-connector . args)
    (match args
      (('handle-message msg)
       ;; TODO: We should indicate to the procedure which calls this that
       ;; the attempt to send the message failed... so, return an 'ok
       ;; or 'failed message here?
       (when (atomic-box-ref running?)
         (send msg #f)))))
  (define am (make-actormap #:vat-connector vat-connector))
  (%make-vat name am running? start halt send))

(define (vat-running? vat)
  "Return #t if VAT is currently running."
  (atomic-box-ref (vat-running vat)))

(define (vat-halt! vat)
  "Stop processing turns for VAT."
  (atomic-box-set! (vat-running vat) #f)
  ((vat-halt-proc vat)))

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
  (define (churn msg)
    (call-with-error-handling
     (lambda ()
       (define-values (returned new-actormap new-msgs)
         (actormap-churn actormap msg))
       (dispatch-messages new-msgs)
       (maybe-merge returned new-actormap)
       returned)
     (lambda (exn)
       `#(fail ,exn))))
  (unless (atomic-box-ref running?)
    (atomic-box-set! running? #t)
    ((vat-start-proc vat) churn)))

(define (vat-send vat msg)
  ((vat-send-proc vat) msg #t))

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
        (define refr (actormap-spawn! am (lambda (_bcom) multi-value-thunk)))
        (match (vat-send vat (make-message refr #f '()))
          (#('ok vals) (apply values vals))
          (#('fail err) (raise-exception err))))
      (error "vat is not running" vat)))

(define-syntax-rule (with-vat vat body ...)
  (call-with-vat vat (lambda () body ...)))

(define* (make-fibrous-vat #:key (name (generate-random-vat-name))
                           (scheduler (default-vat-scheduler))
                           (dynamic-wrap port-redirect-dynamic-wrap))
  (define done? (make-condition))
  (define-values (enq-ch deq-ch stop?)
    (spawn-delivery-agent #:scheduler scheduler))
  (define (start churn)
    (define (handle-message args)
      (match args
        ((msg return-ch)
         ;; We have the put-message be run in its own fiber so that if
         ;; the other side isn't listening for it anymore, the vat
         ;; itself doesn't end up blocked.
         (syscaller-free-fiber
          (lambda ()
            (put-message return-ch (churn msg)))))
        (msg
         (churn msg))))
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
  (define (send msg return?)
    (if return?
        (let ((return-ch (make-channel)))
          (put-message enq-ch (list msg return-ch))
          (get-message return-ch))
        (put-message enq-ch msg)))
  (make-vat #:name name
            #:start start
            #:halt halt
            #:send send))

(define* (spawn-fibrous-vat #:key (name (generate-random-vat-name))
                            (scheduler (default-vat-scheduler))
                            (dynamic-wrap port-redirect-dynamic-wrap))
  (let ((vat (make-fibrous-vat #:name name
                               #:scheduler scheduler
                               #:dynamic-wrap dynamic-wrap)))
    (vat-start! vat)
    vat))

(define* (spawn-vat #:key (name (generate-random-vat-name)))
  (spawn-fibrous-vat #:name name))

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
          (display-backtrace stack (current-error-port))
          (newline (current-error-port))
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
