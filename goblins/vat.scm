;;; Copyright 2021-2022 Christine Lemmer-Webber
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
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 threads)
  #:export (spawn-vat-fiber
            spawn-vat-proc
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
  (set! *random-state* (random-state-from-platform))
  (apply string
	 (map
	  (lambda _ (integer->char (+ 65 (random 26))))
	  (iota 5)))) ;; length.

;; TODO: An explicit 'halt message isn't as ideal as vats which auto-gc.
;; But that is probably possible... we could possibly set up a fializer
;; that is attached to the vat-control-ch and vat-connector of this vat.
(define* (spawn-vat-fiber name #:key (control-ch (make-channel))
                          (scheduler (default-vat-scheduler))
                          (dynamic-wrap port-redirect-dynamic-wrap))
  "Spawns a fiber for this vat and returns a channel by which
you can speak to the vat.

Keywords:
 - control-ch: A control channel by which we will speak to this vat
 - fibrous-io?: (DEPRECATED, to be removed soon) whether or not actors
   suspend to their actor prompt and return a promise when they would
   have blocked
 - scheduler: The Fibers scheduler this vat and its delivery
   agent (for handling incoming messages) will run on
 - dynamic-wrap: Dynamically wrap the launch of the vat, allowing to
   set parameters, etc.  By default redirects current-output-port and
   current-error-port back to their defaults to prevent Geiser evaluation
   screwing up things up."
  (define running? (make-atomic-box #t))
  (define-values (enq-ch deq-ch stop?)
    (spawn-delivery-agent #:scheduler scheduler))
  ;; TODO: Maybe the vat connectors can just be channels sometimes?
  ;; That would simplify this dramatically.  In fact if 'handle-message
  ;; remains the only message, it could just be the enq-ch?
  ;; Oh, except the ability to not block if running? is disabled is kinda
  ;; key, huh!
  (define vat-connector
    (match-lambda*
      (('handle-message msg)
       ;; TODO: We should indicate to the procedure which calls this that
       ;; the attempt to send the message failed... so, return an 'ok
       ;; or 'failed message here?
       (when (atomic-box-ref running?)
         (put-message enq-ch msg)))))
  (define actormap (make-actormap #:vat-connector vat-connector))
  (define (vat-loop)
    ;; Control: operations on the vat from someone who spawned it
    (define handle-vat-control
      ;;      (match-lambda
      (lambda (arg)
        (match arg
          ('halt
           (atomic-box-set! running? #f))
          (('run thunk return-ch)
           (call/ec
            (lambda (abort)
              ;; Yes, we also have slightly similar but almost the same error
              ;; handling here as a bit lower, because we want to return the
              ;; exception to the REPL in case things fail here
              (define (handle-exn exn)
                (define stack
                  (make-stack #t handle-exn))
                (display-backtrace stack (current-error-port))
                (newline (current-error-port))
                (spawn-fiber
                 (lambda ()
                   (put-message return-ch `#(fail ,exn))))
                (abort))
              (define (do-run)
                (define-values (returned new-actormap new-msgs)
                  (actormap-churn-run actormap thunk))
                (dispatch-messages new-msgs)
                (match returned
                  [#('ok rval)
                   (transactormap-merge! new-actormap)]
                  [_ #f])
                ;; we have the put-message be run in its own fiber so that if
                ;; the other side isn't listening for it anymore, the vat
                ;; itself doesn't end up blocked
                (spawn-fiber
                 (lambda ()
                   (put-message return-ch returned))
                 scheduler))
              (with-exception-handler handle-exn
                do-run)))))))
    ;; Connect: operations on the vat from the outside
    (define (handle-incoming-message msg)
      (define-values (returned new-actormap new-msgs)
        (actormap-churn actormap msg))
      (dispatch-messages new-msgs)
      (match returned
        [#('ok rval)
         (transactormap-merge! new-actormap)]
        [_ #f]))
    ;; And now loop doing this...
    (while (atomic-box-ref running?)     ; ... unless it's time to stop
      (call/ec
       (lambda (abort)
         ;; Error handling in case one of these goes badly...
         (define (handle-exn exn)
           (define stack
             (make-stack #t handle-exn))
           (display-backtrace stack (current-error-port))
           (newline (current-error-port))
           (abort))
         ;; Actually run operation
         (define (handle-op)
           (perform-operation
            (choice-operation (wrap-operation (get-operation control-ch)
                                              handle-vat-control)
                              (wrap-operation (get-operation deq-ch)
                                              handle-incoming-message))))
         (with-exception-handler handle-exn
           handle-op)))))
  ;; Wrap vat spawning in "ambient advice", allowing an opportunity
  ;; to set up parameters, etc
  (define _dynamic-wrap
    (or dynamic-wrap (lambda (proc) (proc))))
  (_dynamic-wrap (lambda () (spawn-fiber vat-loop scheduler)))
  running?)

(define* (spawn-vat-proc name #:key
                         (control-ch (make-channel))
                         (dynamic-wrap port-redirect-dynamic-wrap))
  "Like spawn-vat-fiber except returns a convenient procedure which abstracts
over some of the communication aspects of controlling the vat."
  (define running?
    (spawn-vat-fiber name #:control-ch control-ch))
  (define vat-controller
    (match-lambda*
      ((or ((? procedure? thunk)) ('run (? procedure? thunk)))
       (define return-ch (make-channel))
       (put-message control-ch (list 'run thunk return-ch))
       (match (get-message return-ch)
         (#('ok val) val)
         (#('fail err) (raise-exception err))))
      (('halt)
       (put-message control-ch 'halt))
      (('running?)
       (atomic-box-ref running?))))
  vat-controller)

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

(define* (spawn-vat #:key (name #f))
  (let* ((name (or name (generate-random-vat-name)))
	     (result-ch (make-channel))
         (vat-halt? (make-condition))
         (vat-thread
          (call-with-new-thread
           (lambda ()
             (run-fibers
              (lambda ()
                (define a-vat (spawn-vat-proc name))
                (put-message result-ch a-vat)
                (wait vat-halt?))))))
         (new-vat  ; vat controller procedure
          (get-message result-ch)))
    new-vat))

(define-syntax define-vat-run
  (syntax-rules ()
    ((define-vat-run vat-run-id vat)
     (begin
       (define this-vat vat)
       (define-syntax vat-run-id
         (syntax-rules ::: ()
                       ((_ body :::)
                        (this-vat (lambda ()
                                    body :::)))))))
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
