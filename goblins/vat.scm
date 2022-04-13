;;; Copyright 2021 Christine Lemmer-Webber
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
  #:use-module (goblins core)
  #:use-module (goblins inbox)
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (ice-9 match)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 threads)
  #:export (spawn-vat-fiber
            spawn-vat-proc
            spawn-vat
            syscaller-free-fiber

            spawn-fibrous-vow
            fibrous))

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

(define fibers-wait-for-readable
  (@@ (fibers) wait-for-readable))

(define fibers-wait-for-writable
  (@@ (fibers) wait-for-writable))

(define (fibrous-read-waiter port)
  (define (fibrous-fulfill-await resolver)
    (spawn-fiber
     (lambda ()
       (define result (fibers-wait-for-readable port))
       (<-np-extern resolver 'fulfill result))))
  (await* fibrous-fulfill-await))

(define (fibrous-write-waiter port)
  (define (fibrous-fulfill-await resolver)
    (spawn-fiber
     (lambda ()
       (define result (fibers-wait-for-writable port))
       (<-np-extern resolver 'fulfill result))))
  (await* fibrous-fulfill-await))


;; TODO: An explicit 'halt message isn't as ideal as vats which auto-gc.
;; But that is probably possible... we could possibly set up a fializer
;; that is attached to the vat-control-ch and vat-connector of this vat.
;; Once that is gc'ed, it can trigger halting...
;; TODO: Hm, that might not work for vats which do IO, I suppose.
;; At least, not without great care.

(define* (spawn-vat-fiber #:key [fibrous-io? #t])
  "Spawns a fiber for this vat and returns a channel by which
you can speak to the vat."
  (define running? (make-atomic-box #t))
  (define-values (enq-ch deq-ch stop?)
    (spawn-delivery-agent))
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
  (define vat-control-ch (make-channel))
  (define waiters
    (and fibrous-io?
         (cons fibrous-read-waiter fibrous-write-waiter)))
  (define (vat-loop)
    ;; Control: operations on the vat from someone who spawned it
    (define handle-vat-control
      (match-lambda
        (('halt)
         (atomic-box-set! running? #f))
        (('run thunk return-ch)
         (define-values (returned new-actormap new-msgs)
           (actormap-churn-run actormap thunk
                               #:waiters waiters))
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
            (put-message return-ch returned))))))
    ;; Connect: operations on the vat from the outside
    (define (handle-incoming-message msg)
      (define-values (returned new-actormap new-msgs)
        (actormap-churn actormap msg
                        #:waiters waiters))
      (dispatch-messages new-msgs)
      (match returned
        [#('ok rval)
         (transactormap-merge! new-actormap)]
        [_ #f]))
    (while (atomic-box-ref running?)
      (perform-operation
       (choice-operation (wrap-operation (get-operation vat-control-ch)
                                         handle-vat-control)
                         (wrap-operation (get-operation deq-ch)
                                         handle-incoming-message)))))
  (spawn-fiber vat-loop)
  vat-control-ch)

(define* (spawn-vat-proc #:key [fibrous-io? #t])
  "Like spawn-vat-fiber except returns a convenient procedure which abstracts
over some of the communication aspects of controlling the vat."
  (define control-ch (spawn-vat-fiber #:fibrous-io? fibrous-io?))
  (define (vat-runner thunk)
    (define return-ch (make-channel))
    (put-message control-ch (list 'run thunk return-ch))
    (match (get-message return-ch)
      (#('ok val) val)
      (#('fail err) (raise-exception err))))
  vat-runner)

(define (syscaller-free-fiber thunk)
  (syscaller-free
   (lambda ()
     (spawn-fiber thunk))))

(define (spawn-fibrous-vow proc)
  (define-values (promise resolver)
    (spawn-promise-values))
  (spawn-fiber
   (lambda ()
     ;; TODO: Add error handling
     (define result (proc))
     (<-np-extern resolver 'fulfill result)))
  promise)

(define-syntax-rule (fibrous body ...)
  (spawn-fibrous-vow (lambda () body ...)))

(define (spawn-vat)
  (let* ((result-ch (make-channel))
         (vat-halt? (make-condition))
         (repl-thread
          (call-with-new-thread
           (lambda ()
             (run-fibers
              (lambda ()
                (define a-vat (spawn-vat-proc))
                (put-message result-ch a-vat)
                (wait vat-halt?))))))
         (repl-vat  ; vat controller procedure
          (get-message result-ch)))
    repl-vat))

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
