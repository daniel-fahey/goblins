;;; Copyright 2019-2023 Christine Lemmer-Webber
;;; Copyright 2022-2023 David Thompson
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

(define-module (goblins test-vat)
  #:use-module (goblins)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (tests utils)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module ((fibers conditions)
                #:select (make-condition
                          wait-operation
                          signal-condition!))
  #:use-module ((fibers operations)
                #:select (choice-operation
                          perform-operation))
  #:use-module ((fibers timers)
                #:select (sleep-operation))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64))

(test-begin "test-vat")

(define a-vat (spawn-vat))

(test-eq "Lookup vat by id"
  (lookup-vat (vat-id a-vat)) a-vat)

(test-equal "List vats"
  (all-vats) (list a-vat))

(define (^friendo _bcom)
  (lambda ()
    'hello))

(define my-friend
  (with-vat a-vat
    (spawn ^friendo)))

(define (^counter bcom n)
  (lambda ()
    (bcom (^counter bcom (+ n 1)) n)))

(define a-counter
  (with-vat a-vat
   (spawn ^counter 0)))

(define (run vat op . rest)
  (with-vat vat
    (apply op rest)))

(test-eq (run a-vat $ a-counter) 0)
(test-eq (run a-vat $ a-counter) 1)
(test-eq (run a-vat $ a-counter) 2)
(test-eq (run a-vat $ a-counter) 3)
(resolve-vow-and-return-result
 a-vat
 (lambda () (<- a-counter)))
(test-eq (run a-vat $ a-counter) 5)

(define (^counter-poker _bcom counter)
  (lambda ()
    (<-np counter)))
(define counter-poker
  (run a-vat spawn ^counter-poker a-counter))
(test-eq (run a-vat $ a-counter) 6)
(run a-vat $ counter-poker)
(test-eq (run a-vat $ a-counter) 8)
(run a-vat $ counter-poker)
(test-eq (run a-vat $ a-counter) 10)

;; Inter-vat communication
(define b-vat (spawn-vat))
(let ((result
       (resolve-vow-and-return-result
        b-vat
        (lambda () (<- a-counter)))))
  (test-eq (run a-vat $ a-counter) 12))

;; Check inter-vat promise resolution
(let ((result
       (resolve-vow-and-return-result
        b-vat
        (lambda () (<- my-friend)))))
  (test-assert
      "Check promise resolution using on between vats"
    (match result
      (#('ok 'hello) #t)
      (_ #f))))

;; Promise pipelining test
(define (^car-factory _bcom)
  (lambda (color)
    (define (^car _bcom)
      (lambda ()
        (format #f "The ~a car says: *vroom vroom*!" color)))
    (spawn ^car)))
(define car-factory (run a-vat spawn ^car-factory))
(let ((result
       (resolve-vow-and-return-result
        a-vat
        (lambda ()
          (define car-vow (<- car-factory 'green))
          (<- car-vow)))))
  (test-assert
      "Check basic promise pipelining on the same vat works"
    (match result
      (#('ok "The green car says: *vroom vroom*!") #t)
      (_ #f))))

;; Check promise pipelining between vats
(let ((result
       (resolve-vow-and-return-result
        b-vat
        (lambda ()
          (define car-vow (<- car-factory 'red))
          (<- car-vow)))))
  (test-assert
      "Check that basic promise pipeling works between vats"
    (match result
      (#('ok "The red car says: *vroom vroom*!") #t)
      (_ #f))))

;; Test promise pipeling with a broken promise.
(define (^borked-factory _bcom)
  (define (^car _bcom)
    (lambda ()
      (format #f "Vroom vroom")))

  (match-lambda
    ('make-car (spawn ^car))
    ('make-error (error "Oops! no vrooming here :("))))

(define (try-car-pipeline vat factory method-name)
  (resolve-vow-and-return-result
   vat
   (lambda ()
     (define car-vow
       (<- factory method-name))
     (<- car-vow))))

(define borked-factory (run a-vat spawn ^borked-factory))

;; Check the initial working car.
(let ((result (try-car-pipeline a-vat borked-factory 'make-car)))
  (test-assert
      "Sanity check to make sure factory normally works"
    (match result
      (#('ok "Vroom vroom") #t)
      (_ #f))))

(let ((result (try-car-pipeline b-vat borked-factory 'make-car)))
  (test-assert
      "Sanity check to make sure factory normally works across vats"
    (match result
      (#('ok "Vroom vroom") #t)
      (_ #f))))

;; Now check the error.
(let ((result (try-car-pipeline a-vat borked-factory 'make-error)))
  (test-assert
      "Check promise pipeling breaks on error on the same vat"
    (match result
      (#('err _err) #t)
      (_ #f))))

;; Now check that errors work across vats
(let ((result (try-car-pipeline b-vat borked-factory 'make-error)))
  (test-assert
      "Check promise pipeling breaks on error between vats"
    (match result
      (#('err _err) #t)
      (_ #f))))

;;; Literally the version from the Goblins docs

;; Create a "car factory", which makes cars branded with
;; company-name.
(define (^car-factory2 bcom company-name)
  ;; The constructor for cars we will create.
  (define (^car bcom model color)
    (methods                      ; methods for the ^car
     ((drive)                    ; drive the car
      (format #f "*Vroom vroom!*  You drive your ~a ~a ~a!"
              color company-name model))))
  ;; methods for the ^car-factory instance
  (methods                        ; methods for the ^car-factory
   ((make-car model color)       ; create a car
    (spawn ^car model color))))

(define fork-motors
  (with-vat a-vat
   (spawn ^car-factory2 "Fork")))

(define car-vow
  (with-vat b-vat
   (<- fork-motors 'make-car "Explorist" "blue")))

(define car-pipeline-result
  (resolve-vow-and-return-result
   b-vat
   (lambda ()
     (on (<- car-vow 'drive)       ; B->A: send message to future car
         (lambda (val)             ; A->B: result of that message
           (format #f "Heard: ~a\n" val))
         #:promise? #t))))

(test-equal "Make sure promise pipelining works, version 2"
  #(ok "Heard: *Vroom vroom!*  You drive your blue Fork Explorist!\n")
  car-pipeline-result)

(test-equal "Multiple return values from vat invocation"
  (call-with-values (lambda ()
                      (with-vat a-vat
                       (values 1 2 3)))
    list)
  '(1 2 3))

(define (try-far-on-promise . resolve-args)
  (define fulfilled-val #f)
  (define broken-val #f)
  (define finally-ran? #f)
  (define a-promise-and-resolver
    (call-with-vat a-vat spawn-promise-cons))
  (define a-promise (car a-promise-and-resolver))
  (define a-resolver (cdr a-promise-and-resolver))
  (define done? (make-condition))
  (with-vat b-vat
    (on a-promise
        (lambda (val)
          (set! fulfilled-val val))
        #:catch
        (lambda (err)
          (set! broken-val err))
        #:finally
        (lambda ()
          (set! finally-ran? #t)
          (signal-condition! done?))))
  (with-vat a-vat
    (apply $ a-resolver resolve-args))
  ;; Wait until the operation has finished, or one second has
  ;; passed (if this is taking longer than a second that's really
  ;; troubling!)
  (perform-operation (choice-operation
                      (wait-operation done?)
                      (sleep-operation 1)))
  (list fulfilled-val broken-val finally-ran?))

(test-equal
 "On subscription w/ fulfillment to promise on another vat"
 '(yay #f #t)
 (try-far-on-promise 'fulfill 'yay))

(test-equal
 "On subscription w/ breakage to promise on another vat"
 '(#f oh-no #t)
 (try-far-on-promise 'break 'oh-no))

;; Vat event log tests

(let ((t (vat-clock a-vat)))
  (test-eqv "Handling a near message increments the clock"
    (+ t 1)
    (begin
      (with-vat a-vat 'boop)
      (vat-clock a-vat))))

(let ((t (vat-clock a-vat)))
  (test-eqv "Handling a far message syncs the clock before incrementing"
    (+ t 7)
    (let ((msg (make-message 'fake-vat my-friend #f '())))
      ((vat-connector a-vat) 'handle-message (+ t 5) msg)
      ;; Making a no-op call into the vat to ensure that the prior
      ;; message has been processed.
      (with-vat a-vat 'boop)
      (vat-clock a-vat))))

(test-eqv "No events are recorded when logging is disabled"
  0
  (begin
    (vat-log-clear! a-vat)
    (set-vat-logging! a-vat #f)
    (with-vat a-vat 'boop)
    (vat-log-length a-vat)))

(test-eqv "Events are recorded when logging is enabled"
  1
  (begin
    (vat-log-clear! a-vat)
    (set-vat-logging! a-vat #t)
    (with-vat a-vat 'boop)
    (vat-log-length a-vat)))

(test-assert "Events can be looked up by log index"
  (begin
    (vat-log-clear! a-vat)
    (set-vat-logging! a-vat #t)

    (with-vat a-vat 'boop)
    (vat-event? (vat-log-ref a-vat 0))))

(test-assert "Events can be looked up by timestamp"
  (let ((t (vat-clock a-vat)))
    (vat-log-clear! a-vat)
    (set-vat-logging! a-vat #t)
    (with-vat a-vat 'boop)
    (vat-event? (vat-log-ref-by-time a-vat (+ t 1)))))

(test-assert "Events can be looked up by message"
  (let ((t (vat-clock a-vat))
        (msg (make-message 'fake-vat my-friend #f '())))
    (vat-log-clear! a-vat)
    (set-vat-logging! a-vat #t)
    ((vat-connector a-vat) 'handle-message t msg)
    ;; Making a no-op call into the vat to ensure that the prior
    ;; message has been processed.
    (with-vat a-vat 'boop)
    (vat-event? (vat-log-ref-by-message a-vat msg))))

(test-assert "The previous event in a churn can be looked up"
  (let ((t (vat-clock a-vat)))
    (vat-log-clear! a-vat)
    (set-vat-logging! a-vat #t)
    (with-vat a-vat ;; (+ t 1)
      (<- my-friend) ;; (+ t 2)
      (<- my-friend)) ;; (+ t 3)
    (let ((prev (vat-log-ref-by-time a-vat (+ t 2)))
          (event (vat-log-ref-by-time a-vat (+ t 3))))
      (and (vat-event? prev)
           (vat-event? event)
           (eq? prev (vat-log-ref-previous a-vat event))))))

(test-assert "The next event in a churn can be looked up"
  (let ((t (vat-clock a-vat)))
    (vat-log-clear! a-vat)
    (set-vat-logging! a-vat #t)
    (with-vat a-vat ;; (+ t 1)
      (<- my-friend) ;; (+ t 2)
      (<- my-friend)) ;; (+ t 3)
    (let ((event (vat-log-ref-by-time a-vat (+ t 2)))
          (next (vat-log-ref-by-time a-vat (+ t 3))))
      (and (vat-event? event)
           (vat-event? next)
           (eq? next (vat-log-ref-next a-vat event))))))

(test-assert "Event information can be obtained from vat errors"
  (let ((t (vat-clock a-vat)))
    (define (handle-error e)
      (let ((event (vat-log-ref-by-time a-vat (+ t 1))))
        (eq? event (vat-turn-error-event e))))
    (vat-log-clear! a-vat)
    (set-vat-logging! a-vat #t)
    (with-exception-handler handle-error
      (lambda ()
        (with-vat a-vat
          (+ 1 "two")))
      #:unwind? #t
      #:unwind-for-type &vat-turn-error)))

(test-assert "Errors associated with events can be looked up"
  (let ((t (vat-clock a-vat)))
    (define (handle-error e)
      (let ((event (vat-log-ref-by-time a-vat (+ t 1))))
        (eq? e (vat-log-error-for-event a-vat event))))
    (vat-log-clear! a-vat)
    (set-vat-logging! a-vat #t)
    (with-exception-handler handle-error
      (lambda ()
        (with-vat a-vat
          (+ 1 "two")))
      #:unwind? #t
      #:unwind-for-type &vat-turn-error)))

(test-equal "Historical actormap state can be queried via event snapshots"
  '(gold sword)
  (let ((t (vat-clock a-vat))
        (chest (with-vat a-vat (spawn ^cell 'gold)))) ;; (+ t 1)
    (with-vat a-vat ;; (+ t 2)
      (on (<- chest 'sword) ;; (+ t 3)
          (lambda _ 'no-op))) ;; (+ t 7)
    ;; Compare before and after the event that modified the contents
    ;; of the cell.
    (let ((before-cell-update-event (vat-log-ref-by-time a-vat (+ t 3)))
          (after-cell-update-event (vat-log-ref-by-time a-vat (+ t 4))))
      (list (actormap-peek (vat-event-snapshot before-cell-update-event)
                           chest)
            (actormap-peek (vat-event-snapshot after-cell-update-event)
                           chest)))))

(test-assert "Event log activation order backtrace across vats"
  (begin
    (vat-log-clear! a-vat)
    (vat-log-clear! b-vat)
    (set-vat-logging! a-vat #t)
    (set-vat-logging! b-vat #t)
    (let* ((counter (with-vat b-vat (spawn ^counter 0)))
           (done? (make-condition))
           (t (vat-clock a-vat))
           (vow (with-vat a-vat ;; (+ t 1)
                  (on (<- counter)
                      #:finally (lambda ()
                                  (signal-condition! done?))))))
      ;; Wait for the promise to resolve.
      (perform-operation (choice-operation
                          (wait-operation done?)
                          (sleep-operation 1)))
      ;; Get the backtrace of the promise resolution event and verify
      ;; that the initial event is the with-vat message.  The trace
      ;; has to go from vat A -> B -> A to get this result.
      (let* ((finally-event (vat-log-ref-by-time a-vat (vat-clock a-vat))))
        (eq? (vat-log-ref-by-time a-vat (+ t 1))
             (car (reverse (vat-event-trace finally-event))))))))

(test-assert "Event log message order tree across vats"
  (begin
    (vat-log-clear! a-vat)
    (vat-log-clear! b-vat)
    (set-vat-logging! a-vat #t)
    (set-vat-logging! b-vat #t)
    (let* ((counter (with-vat b-vat (spawn ^counter 0)))
           (done? (make-condition))
           (t (vat-clock a-vat))
           (vow (with-vat a-vat ;; (+ t 1)
                  (on (<- counter)
                      #:finally (lambda ()
                                  (signal-condition! done?))))))
      ;; Wait for the promise to resolve.
      (perform-operation (choice-operation
                          (wait-operation done?)
                          (sleep-operation 1)))
      ;; Get the tree of the promise resolution event and verify that
      ;; the first event at depth 0 is the with-vat message.  The
      ;; trace has to go from vat A -> B -> A to get this result.
      (let* ((finally-event (vat-log-ref-by-time a-vat (vat-clock a-vat))))
        (eq? (vat-log-ref-by-time a-vat (+ t 1))
             (car (vat-event-tree finally-event)))))))

;; Running this test last since it messes with the log size.
(test-assert "The event log can be resized"
  (let ((t (vat-clock a-vat)))
    (vat-log-clear! a-vat)
    (set-vat-logging! a-vat #t)
    (with-vat a-vat 'beep) ;; (+ t 1)
    (with-vat a-vat 'boop) ;; (+ t 2)
    (vat-log-resize! a-vat 1)
    (and (= (vat-log-length a-vat) 1)
         ;; Event doesn't fit in resized log and is dropped.
         (not (vat-log-ref-by-time a-vat (+ t 1)))
         ;; The last event is still there, though.
         (vat-event? (vat-log-ref-by-time a-vat (+ t 2))))))

(test-end "test-vat")
