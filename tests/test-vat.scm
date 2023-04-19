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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64))

(test-begin "test-vat")

(define a-vat (spawn-vat #:name 'A))

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
(define b-vat (spawn-vat #:name 'B))
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
      (<- my-friend)) ;; (+ t 2)
    (let ((prev (vat-log-ref-by-time a-vat (+ t 1)))
          (event (vat-log-ref-by-time a-vat (+ t 2))))
      (and (vat-event? prev)
           (vat-event? event)
           (eq? prev (vat-log-ref-previous a-vat event))))))

(test-assert "The next events in a churn can be looked up"
  (let ((t (vat-clock a-vat)))
    (vat-log-clear! a-vat)
    (set-vat-logging! a-vat #t)
    (with-vat a-vat ;; (+ t 1)
      (<- my-friend) ;; (+ t 2)
      (<- my-friend)) ;; (+ t 3)
    (let ((event (vat-log-ref-by-time a-vat (+ t 1)))
          (next (list (vat-log-ref-by-time a-vat (+ t 2))
                      (vat-log-ref-by-time a-vat (+ t 3)))))
      (and (vat-event? event)
           (every vat-event? next)
           (equal? next (vat-log-ref-next a-vat event))))))

;; Test for https://gitlab.com/spritely/guile-goblins/-/issues/101
(test-equal "Root events are not added to the 'next' index"
  '()
  (begin
    (vat-log-clear! a-vat)
    (set-vat-logging! a-vat #t)
    (with-vat a-vat 'no-op)
    ;; A root event has a previous event of #f, or no event.  That
    ;; shouldn't mean that the next event list for "event" #f is a
    ;; list of all root events.  It should be the empty list.
    (vat-log-ref-next a-vat #f)))

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
  '(#f gold)
  ;; Events:
  ;; 1) with-vat to spawn chest
  ;; 2) with-vat to send messages to chest
  ;; 3) update chest contents from #f to 'gold'
  ;; 4) update chest contents from 'gold' to 'sword'
  (let* ((t (vat-clock a-vat))
         (chest (with-vat a-vat (spawn ^cell #f))))
    (define (peek timestamp)
      (actormap-peek (vat-event-snapshot
                      (vat-log-ref-by-time a-vat timestamp))
                     chest))
    (with-vat a-vat
      (<-np chest 'gold)
      (<-np chest 'sword))
    (list (peek (+ t 3))
          (peek (+ t 4)))))

(test-assert "Far message snapshots reflect the state at the end of the turn that caused them"
  (let ((t (vat-clock a-vat))
        ;; This chest has a sneaky setter method.  The cell contents
        ;; are updated synchronously to hold the new value, but then
        ;; the cell is updated asynchronously to hold a shield.
        ;; That's a pretty weird thing to do, but we're doing it to
        ;; tease out an important detail surrounding vat event
        ;; snapshots.  For a far message, the snapshot should be the
        ;; state of the actormap at the end of the turn that caused
        ;; it.  The sneaky chest gives us a scenario where the state
        ;; of the cell when the setter returns is different than its
        ;; state at both the beginning and end of the churn.
        (sneaky-chest
         (with-vat a-vat
           (let ((cell (spawn ^cell #f)))
             (define (^sneaky-cell _bcom)
               (case-lambda
                 (()
                  ($ cell))
                 ((new-val)
                  ;; Immediately store the requested value but...
                  ($ cell new-val)
                  ;; ...sneakily overwrite the contents of the cell
                  ;; asynchronously!  This message will be processed
                  ;; during the same churn.
                  (<-np cell 'shield)
                  ;; Return cell contents.
                  ($ cell))))
             (spawn ^sneaky-cell)))))
    ;; Try to put a sword in the chest, but that sneaky chest has
    ;; other plans!
    (resolve-vow-and-return-result
     b-vat
     (lambda () (<- sneaky-chest 'sword)))
    ;; Vat A events:
    ;; 1) with-vat to spawn sneaky-chest
    ;; 2) update sneaky-chest with 'sword' as requested by vat A
    ;; 3) update sneaky-chest with 'shield'
    ;; 4) send promise resolution value of 'sword' to vat B
    (let ((event (vat-log-ref-by-time a-vat (+ t 4))))
      (and (vat-send-event? event)
           (eq? (actormap-peek (vat-event-snapshot event) sneaky-chest)
                'sword)))))

(test-assert "Events with listen requests satisfy vat-event-listen? predicate"
  ;; (+ t 1): call-with-vat to spawn counter
  ;; (+ t 2): call-with-vat to increment counter
  ;; (+ t 3): (<- counter)
  ;; (+ t 4): listen to promise
  (let ((t (vat-clock a-vat))
        (counter (with-vat a-vat (spawn ^counter 0))))
    (resolve-vow-and-return-result
     a-vat
     (lambda ()
       (on (<- counter) identity)))
    (and (vat-event-listen? (vat-log-ref-by-time a-vat (+ t 4)))
         (not (vat-event-listen? (vat-log-ref-by-time a-vat (+ t 1)))))))

(test-assert "Events with messages satisfy vat-event-message? predicate"
  (begin
    (with-vat a-vat 'no-op)
    (vat-event-message? (vat-log-ref-by-time a-vat (vat-clock a-vat)))))

(test-assert "Events sent to local objects satisfy vat-event-local? predicate"
  (begin
    (with-vat a-vat 'no-op)
    (vat-event-local? (vat-log-ref-by-time a-vat (vat-clock a-vat)))))

(test-assert "Event log activation order backtrace across vats"
  (begin
    (vat-log-clear! a-vat)
    (vat-log-clear! b-vat)
    (set-vat-logging! a-vat #t)
    (set-vat-logging! b-vat #t)
    (let* ((counter (with-vat b-vat (spawn ^counter 0)))
           (done? (make-condition))
           ;; The start time for vat A.
           (ta1 (vat-clock a-vat))
           ;; The start time for b-vat when it receives a message from
           ;; vat A.  Vat A ticks its clock twice before vat B is
           ;; involved: Once for the call-with-vat call, and once more
           ;; to send a message to vat B. Therefore, due to the
           ;; Lamport clock logic, B's clock will advance to (+ ta1 2)
           ;; if it is greater than B's current clock value.
           (tb (max (+ ta1 2) (vat-clock b-vat)))
           ;; The start time for vat A when it first receives a
           ;; promise fulfillment message from vat B.  Vat B ticks its
           ;; own clock twice before that happens (receiving a message
           ;; and sending a promise resolve message), and vat A ticks
           ;; once for a listen request, for a total of a three tick
           ;; offset.
           (ta2 (+ tb 3)))
      (with-vat a-vat
        ;; This is a distractor promise.  It is part of the
        ;; same churn as the promise below, but it shouldn't
        ;; show up in the trace.
        (on (<- counter) (const #t))
        ;; This is the promise we want to trace, starting
        ;; from the #:finally handler and back to the
        ;; 'with-vat' that kicked off the process.
        (on (<- counter)
            #:finally
            (lambda ()
              (signal-condition! done?))))
      ;; Wait for the promise to resolve.
      (perform-operation (wait-operation done?))
      (let ((e0 (vat-log-ref-by-time a-vat (+ ta1 1)))  ; A: recv: with-vat
            ;; Outside of the trace:
            ;; (+ ta1 2): distractor (<- counter) message send to vat B.
            ;; (+ ta1 3): listen request for the above promise.
            (e1 (vat-log-ref-by-time a-vat (+ ta1 4)))  ; A: send: (<- counter)
            ;; Outside of the trace:
            ;; (+ ta1 5): listen request for the promise.
            ;; (+ tb 1): receive distractor (<- counter) message.
            ;; (+ tb 2): send resolver fulfill message back to vat A.
            (e2 (vat-log-ref-by-time b-vat (+ tb 3)))   ; B: recv: (<- counter)
            (e3 (vat-log-ref-by-time b-vat (+ tb 4)))   ; B: send: resolve
            ;; Outside of the trace:
            ;; (+ ta2 1): receive distractor promise resolution from vat B.
            ;; (+ ta2 2): fulfill listener for distractor promise.
            ;; (+ ta2 3): call fulfilled handler for distractor promise.
            (e4 (vat-log-ref-by-time a-vat (+ ta2 4)))  ; A: recv: resolve
            (e5 (vat-log-ref-by-time a-vat (+ ta2 5)))  ; A: recv: fulfill
            (e6 (vat-log-ref-by-time a-vat (+ ta2 6)))) ; A: recv: finally
        ;; Get the backtrace of the promise resolution event and
        ;; verify that it matches our expectation.  The trace has to
        ;; go from vat A -> B -> A to get the correct result.
        (equal? (list e6 e5 e4 e3 e2 e1 e0) ; trace goes backwards in time
                (vat-event-trace (vat-log-ref-by-time a-vat (vat-clock a-vat))))))))

(test-assert "Event log message order tree across vats"
  (begin
    (vat-log-clear! a-vat)
    (vat-log-clear! b-vat)
    (set-vat-logging! a-vat #t)
    (set-vat-logging! b-vat #t)
    (let* ((counter (with-vat b-vat (spawn ^counter 0)))
           (done? (make-condition))
           ;; The start time for vat A.
           (ta1 (vat-clock a-vat))
           ;; The start time for b-vat when it receives a message from
           ;; a-vat.  Vat A processes two messages before vat receives
           ;; a message: The with-vat message, and the send for
           ;; messaging the counter in vat B.
           (tb (max (+ ta1 2) (vat-clock b-vat)))
           ;; The start time for vat A when it first receives a a
           ;; message back from vat B.  Vat B ticks its own clock
           ;; twice before that happens (receiving a message for the
           ;; counter, the sending a promise resolution message back),
           ;; and vat A in the meantime has processed another listen
           ;; request, so the total offset is 3 ticks.
           (ta2 (+ tb 3)))
      (with-vat a-vat
        (on (<- counter) (const #t))
        (on (<- counter)
            #:finally
            (lambda ()
              (signal-condition! done?))))
      ;; Wait for the promise to resolve.
      (perform-operation (wait-operation done?))
      ;; Get the tree of the promise resolution event and verify that
      ;; it matches what we expect.
      (let ((e0 (vat-log-ref-by-time a-vat (+ ta1 1)))   ; A: recv: with-vat
            (e1 (vat-log-ref-by-time a-vat (+ ta1 2)))   ; A: send: (<- counter)
            (e2 (vat-log-ref-by-time a-vat (+ ta1 3)))   ; A: recv: listen
            (e3 (vat-log-ref-by-time a-vat (+ ta1 4)))   ; A: send: (<- counter)
            (e4 (vat-log-ref-by-time a-vat (+ ta1 5)))   ; A: recv: listen
            (e5 (vat-log-ref-by-time b-vat (+ tb 1)))    ; B: recv: (<- counter)
            (e6 (vat-log-ref-by-time b-vat (+ tb 2)))    ; B: send: resolve
            (e7 (vat-log-ref-by-time b-vat (+ tb 3)))    ; B: recv: (<- counter)
            (e8 (vat-log-ref-by-time b-vat (+ tb 4)))    ; B: send: resolve
            (e9 (vat-log-ref-by-time a-vat (+ ta2 1)))   ; A: recv: resolve
            (e10 (vat-log-ref-by-time a-vat (+ ta2 2)))  ; A: recv: fulfill
            (e11 (vat-log-ref-by-time a-vat (+ ta2 3)))  ; A: recv: handler
            (e12 (vat-log-ref-by-time a-vat (+ ta2 4)))  ; A: recv: resolve
            (e13 (vat-log-ref-by-time a-vat (+ ta2 5)))  ; A: recv: fulfill
            (e14 (vat-log-ref-by-time a-vat (+ ta2 6)))) ; A: recv: handler
        ;; This weird looking thing is the vat tree we are expecting.
        (equal? `(,e0
                  (,e1
                   (,e5
                    (,e6
                     (,e9
                      (,e10
                       ,e11)))))
                  ,e2
                  (,e3
                   (,e7
                    (,e8
                     (,e12
                      (,e13
                       ,e14)))))
                  ,e4)
                (vat-event-tree
                 (vat-log-ref-by-time a-vat (vat-clock a-vat))))))))

(test-assert "Event log message order tree with partial log"
  ;; This test ensures that tree construction doesn't fail when a vat
  ;; doesn't have the event information we are looking for.  In order
  ;; to test this properly, we need a trace that doesn't have events
  ;; from vat B in it, but a tree that *does*.
  (begin
    (vat-log-clear! a-vat)
    (vat-log-clear! b-vat)
    ;; A logs but B doesn't.
    (set-vat-logging! a-vat #t)
    (set-vat-logging! b-vat #f)
    (let* ((done? (make-condition))
           (a-counter (with-vat a-vat (spawn ^counter 0)))
           (b-counter (with-vat b-vat (spawn ^counter 0)))
           (t (vat-clock a-vat)))
      (with-vat a-vat
        ;; This puts events from vat B within the event tree of this
        ;; 'with-vat' form, but *outside* of the trace of the next
        ;; 'on' form.
        (on (<- b-counter)
            (lambda _
              (signal-condition! done?)))
        ;; Local async message.
        (on (<- a-counter)
            (const #t)))
      ;; Wait for the promise for b-counter to resolve.
      (perform-operation (wait-operation done?))
      ;; Get the tree of the a-counter promise resolution event and
      ;; verify that it matches what we expect.
      (let ((e0 (vat-log-ref-by-time a-vat (+ t 1))) ; A: recv: with-vat
            (e1 (vat-log-ref-by-time a-vat (+ t 2))) ; A: send: (<- b-counter))
            (e2 (vat-log-ref-by-time a-vat (+ t 3))) ; A: recv: listen
            (e3 (vat-log-ref-by-time a-vat (+ t 4))) ; A: recv: (<- a-counter)
            (e4 (vat-log-ref-by-time a-vat (+ t 5))) ; A: recv: listen
            (e5 (vat-log-ref-by-time a-vat (+ t 6))) ; A: recv: resolver fulfill
            (e6 (vat-log-ref-by-time a-vat (+ t 7))) ; A: recv: promise fulfill
            (e7 (vat-log-ref-by-time a-vat (+ t 8)))) ; A: recv: fulfilled-handler
        (equal? `(,e0
                  ,e1
                  ,e2
                  (,e3
                   (,e5
                    (,e6
                     ,e7)))
                  ,e4)
                (vat-event-tree e7))))))

(test-assert "Mapping event tree can modify tree structure"
  (begin
    (vat-log-clear! a-vat)
    (vat-log-clear! b-vat)
    (set-vat-logging! a-vat #t)
    (set-vat-logging! b-vat #t)
    (let* ((counter (with-vat b-vat (spawn ^counter 0)))
           ;; The root of the event tree is in vat A, so our timer
           ;; starts with vat A's current time.
           (ta (vat-clock a-vat))
           ;; Vat A ticks its clock twice, once for receiving the
           ;; with-vat message, and again to send a message to the
           ;; counter.  Therefore, due to the Lamport clock logic, B's
           ;; clock will advance to (+ ta 2) if it is greater than B's
           ;; current clock value.
           (tb (max (vat-clock b-vat) (+ ta 2))))
      ;; Send a message with no promise to minimize vat events.
      ;; Opting not to use resolve-vow-and-return-result here as it
      ;; generates more events to deal with.
      (with-vat a-vat (<-np counter))
      ;; This is just to sync up with b-vat before proceeding with the
      ;; test.
      (resolve-vow-and-return-result a-vat (lambda () (<- counter)))
      (let ((e0 (vat-log-ref-by-time a-vat (+ ta 1))) ; A: recv: with-vat
            (e1 (vat-log-ref-by-time a-vat (+ ta 2))) ; A: send: (<- counter)
            (e2 (vat-log-ref-by-time b-vat (+ tb 1)))) ; B: recv: (<- counter)
        ;; Remove the send event from the tree, preserving the
        ;; associated receive event.
        (equal? `(,e0 ,e2)
                (vat-event-tree-map (match-lambda
                                      (((? vat-send-event?) next-event)
                                       next-event)
                                      (other other))
                                    (vat-event-tree e2)))))))

(test-assert "Mapping event tree with the identity procedure returns the same tree"
  (begin
    (vat-log-clear! a-vat)
    (set-vat-logging! a-vat #t)
    (let ((t (vat-clock a-vat))
          (counter (with-vat a-vat (spawn ^counter 0))))
      (resolve-vow-and-return-result
       a-vat
       (lambda ()
         (on (<- counter) identity)))
      (let ((tree (vat-event-tree
                   (vat-log-ref-by-time a-vat (vat-clock a-vat)))))
        (equal? tree (vat-event-tree-map identity tree))))))

(test-assert "Filtering event tree keeps only nodes that satisfy predicate"
  (begin
    (vat-log-clear! a-vat)
    (vat-log-clear! b-vat)
    (set-vat-logging! a-vat #t)
    (set-vat-logging! b-vat #t)
    (let* ((counter (with-vat b-vat (spawn ^counter 0)))
           ;; The root of the event tree is in vat A, so our timer
           ;; starts with vat A's current time.  This is the first of
           ;; two clock values for vat A because the sequence of
           ;; events progresses from vat A, to vat B, and then back to
           ;; vat A.
           (ta1 (vat-clock a-vat))
           ;; Vat A ticks its clock twice, once for receiving the
           ;; with-vat message, and once more to send a message to the
           ;; counter.  Therefore, due to the Lamport clock logic, B's
           ;; clock will advance to (+ ta 2) if it is greater than B's
           ;; current clock value.
           (tb (max (vat-clock b-vat) (+ ta1 2)))
           ;; With the clocks synced between vat A and B, vat B ticks
           ;; its clock twice.  Once to receive the message to the
           ;; counter, and once more to send a message to the promise
           ;; resolver in vat A.  Vat A has not processed any other
           ;; messages in the meantime, so receiving the next message
           ;; from vat B will advance vat A's clock to (+ tb 2).
           (ta2 (+ tb 2))
           (vow (with-vat a-vat (on (<- counter) identity))))
      (resolve-vow-and-return-result a-vat (lambda () vow))
      (let ((e0 (vat-log-ref-by-time a-vat (+ ta1 1)))  ; A: recv: with-vat
            (e1 (vat-log-ref-by-time a-vat (+ ta1 2)))  ; A: send: (<- counter)
            ;; This listen event will be filtered out.
            (e2 (vat-log-ref-by-time a-vat (+ ta1 3)))  ; A: recv: listen
            (e3 (vat-log-ref-by-time b-vat (+ tb 1)))   ; B: recv: (<- counter)
            (e4 (vat-log-ref-by-time b-vat (+ tb 2)))   ; B: send: resolver fulfill
            (e5 (vat-log-ref-by-time a-vat (+ ta2 1)))  ; A: recv: resolver fulfill
            (e6 (vat-log-ref-by-time a-vat (+ ta2 2)))  ; A: recv: listener fulfill
            (e7 (vat-log-ref-by-time a-vat (+ ta2 3)))) ; A: recv: fulfilled handler
        ;; Keep only message events, removing e1, the only listen
        ;; request event.
        (equal? `(,e0 (,e1 (,e3 (,e4 (,e5 (,e6 ,e7))))))
                (vat-event-tree-filter (match-lambda
                                         ((? vat-event? event)
                                          (vat-event-message? event))
                                         (_ #t))
                                       (vat-event-tree e7)))))))

(test-assert "Filtering event tree with an always true predicate returns the same tree"
  (begin
    (vat-log-clear! a-vat)
    (set-vat-logging! a-vat #t)
    (let ((t (vat-clock a-vat))
          (counter (with-vat a-vat (spawn ^counter 0))))
      (resolve-vow-and-return-result
       a-vat
       (lambda ()
         (on (<- counter) identity)))
      (let ((tree (vat-event-tree
                   (vat-log-ref-by-time a-vat (vat-clock a-vat)))))
        (equal? tree (vat-event-tree-filter (const #t) tree))))))

(test-assert "vat trees can be converted to timeline graphs"
  (begin
    (vat-log-clear! a-vat)
    (vat-log-clear! b-vat)
    (set-vat-logging! a-vat #t)
    (set-vat-logging! b-vat #t)
    (let* ((counter (with-vat b-vat (spawn ^counter 0)))
           ;; The root of the event tree is in vat A, so our timer
           ;; starts with vat A's current time.  This is the first of
           ;; two clock values for vat A because the sequence of
           ;; events progresses from vat A, to vat B, and then back to
           ;; vat A.
           (ta1 (vat-clock a-vat))
           ;; Vat A ticks its clock two times before B receives a
           ;; message. Once for receiving the with-vat message, and
           ;; once more to send a message to the counter.  Therefore,
           ;; due to the Lamport clock logic, B's clock will advance
           ;; to (+ ta 2) if it is greater than B's current clock
           ;; value.
           (tb (max (vat-clock b-vat) (+ ta1 2)))
           ;; With the clocks synced between vat A and B, vat B ticks
           ;; its clock twice.  Once to receive the message to the
           ;; counter, and once more to send a message to the promise
           ;; resolver in vat A.
           (ta2 (+ tb 2))
           (vow (with-vat a-vat (on (<- counter) identity))))
      (resolve-vow-and-return-result a-vat (lambda () vow))
      (let ((e0 (vat-log-ref-by-time a-vat (+ ta1 1)))  ; A: recv: with-vat
            (e1 (vat-log-ref-by-time a-vat (+ ta1 2)))  ; A: send: (<- counter)
            (e2 (vat-log-ref-by-time a-vat (+ ta1 3)))  ; A: recv: listen
            (e3 (vat-log-ref-by-time b-vat (+ tb 1)))   ; B: recv: (<- counter)
            (e4 (vat-log-ref-by-time b-vat (+ tb 2)))   ; B: send: resolver fulfill
            (e5 (vat-log-ref-by-time a-vat (+ ta2 1)))  ; A: recv: resolver fulfill
            (e6 (vat-log-ref-by-time a-vat (+ ta2 2)))  ; A: recv: listener fulfill
            (e7 (vat-log-ref-by-time a-vat (+ ta2 3)))) ; A: recv: fulfilled handler
        (equal? (list (cons (vat-connector a-vat)
                            (list (cons (vat-event-timestamp e0)
                                        (list e0))
                                  (cons (vat-event-timestamp e1)
                                        (list e1 (list (vat-connector b-vat)
                                                       (vat-event-timestamp e3))))
                                  (cons (vat-event-timestamp e2)
                                        (list e2))
                                  (cons (vat-event-timestamp e5)
                                        (list e5))
                                  (cons (vat-event-timestamp e6)
                                        (list e6))
                                  (cons (vat-event-timestamp e7)
                                        (list e7))))
                      (cons (vat-connector b-vat)
                            (list (cons (vat-event-timestamp e3)
                                        (list e3))
                                  (cons (vat-event-timestamp e4)
                                        (list e4 (list (vat-connector a-vat)
                                                       (vat-event-timestamp e5)))))))
                ;; Convert hash table to an alist, sorting the keys
                ;; for a deterministic result.
                (sort (hash-fold (lambda (vat-connector events result)
                                   (acons vat-connector
                                          (sort (hash-fold acons '() events)
                                                ;; Keys are integers,
                                                ;; so < is enough.
                                                (lambda (a b)
                                                  (< (car a) (car b))))
                                          result))
                                 '()
                                 (vat-event-tree->timeline (vat-event-tree e7)))
                      ;; Keys are vat connector procedures, so compare
                      ;; vat names.
                      (lambda (a b)
                        (string< (symbol->string ((car a) 'name))
                                 (symbol->string ((car b) 'name))))))))))

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
