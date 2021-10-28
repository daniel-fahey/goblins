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
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (ice-9 match)
  #:use-module (ice-9 atomic)
  #:export (spawn-vat-fiber
            spawn-vat))

;; TODO: An explicit 'halt message isn't as ideal as vats which auto-gc.
;; But that is probably possible... we could possibly set up a fializer
;; that is attached to the vat-control-ch and vat-connector of this vat.
;; Once that is gc'ed, it can trigger halting...
;; TODO: Hm, that might not work for vats which do IO, I suppose.
;; At least, not without great care.

(define (spawn-vat-fiber)
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
  (define (vat-loop)
    ;; Control: operations on the vat from someone who spawned it
    (define handle-vat-control
      (match-lambda
        (('halt)
         (atomic-box-set! running? #f))
        (('run thunk return-ch)
         (define-values (returned new-actormap new-msgs)
           (actormap-churn-run actormap thunk))
         (dispatch-messages new-msgs)
         (match returned
           [#('ok rval)
            (transactormap-merge! new-actormap)]
           [_ #f])
         (put-message return-ch returned))
        ))
    ;; Connect: operations on the vat from the outside
    (define (handle-incoming-message msg)
      (define-values (returned new-actormap new-msgs)
        (actormap-churn actormap msg))
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

(define (spawn-vat)
  "Like spawn-vat-fiber except returns a convenient procedure which abstracts
over some of the communication aspects of controlling the vat."
  (define control-ch (spawn-vat-fiber))
  (define vat-controller
    (match-lambda*
      (('run thunk)
       (define return-ch (make-channel))
       (put-message control-ch (list 'run thunk return-ch))
       (match (get-message return-ch)
         (#('ok val) val)
         (#('fail err) (raise-exception err))))
      (('halt)
       (put-message control-ch 'halt))))
  vat-controller)

