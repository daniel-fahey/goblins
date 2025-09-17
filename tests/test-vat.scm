;;; Copyright 2019-2023 Christine Lemmer-Webber
;;; Copyright 2022-2023 David Thompson
;;; Copyright 2022-2024 Jessica Tallon
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

(define-module (tests test-vat)
  #:use-module (goblins core)
  #:use-module (goblins core-types)
  #:use-module (goblins abstract-types)
  #:use-module (goblins migrations)
  #:use-module (goblins define-actor)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins persistence-store memory)
  #:use-module (goblins utils hashmap)
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
  a-vat
  (lookup-vat (vat-id a-vat)))

(test-equal "List vats"
  (list a-vat)
  (all-vats))

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

(test-eq 0 (run a-vat $ a-counter))
(test-eq 1 (run a-vat $ a-counter))
(test-eq 2 (run a-vat $ a-counter))
(test-eq 3 (run a-vat $ a-counter))
(resolve-vow-and-return-result
 a-vat
 (lambda () (<- a-counter)))
(test-eq 5 (run a-vat $ a-counter))

(define (^counter-poker _bcom counter)
  (lambda ()
    (<-np counter)))
(define counter-poker
  (run a-vat spawn ^counter-poker a-counter))
(test-eq 6 (run a-vat $ a-counter))
(run a-vat $ counter-poker)
(test-eq 8 (run a-vat $ a-counter))
(run a-vat $ counter-poker)
(test-eq 10 (run a-vat $ a-counter))

;; Inter-vat communication
(define b-vat (spawn-vat #:name 'B))
(let ((result
       (resolve-vow-and-return-result
        b-vat
        (lambda () (<- a-counter)))))
  (test-eq 12 (run a-vat $ a-counter)))

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
  '(1 2 3)
  (call-with-values (lambda ()
                      (with-vat a-vat
                       (values 1 2 3)))
    list))

(define (try-far-on-promise . resolve-args)
  (define fulfilled-val #f)
  (define broken-val #f)
  (define finally-ran? #f)
  (define-values (a-promise a-resolver)
    (call-with-vat a-vat spawn-promise-and-resolver))
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

(test-equal "On subscription w/ fulfillment to promise on another vat"
 '(yay #f #t)
 (try-far-on-promise 'fulfill 'yay))

(test-equal "On subscription w/ breakage to promise on another vat"
 '(#f oh-no #t)
 (try-far-on-promise 'break 'oh-no))

(test-equal "await works within a vat"
  'hello
  (let ((result #f))
    (with-vat a-vat
      (let ((friend (spawn ^friendo)))
        (set! result (<<- friend))))
    result))

(test-equal "the *awaited* value can be returned from call-with-vat"
  '*awaited*
  (with-vat a-vat
    (let ((friend (spawn ^friendo)))
      (<<- friend))))

(define (^greeter _bcom my-name)
  (lambda (your-name)
    (format #f "Hello ~a, I'm ~a"
            your-name my-name)))

(test-equal "Multiple messages dispatched at once between vats resolve"
  '("Hello Bob0, I'm Alice"
    "Hello Bob1, I'm Alice"
    "Hello Bob2, I'm Alice"
    "Hello Bob3, I'm Alice"
    "Hello Bob4, I'm Alice"
    "Hello Bob5, I'm Alice"
    "Hello Bob6, I'm Alice"
    "Hello Bob7, I'm Alice"
    "Hello Bob8, I'm Alice"
    "Hello Bob9, I'm Alice")
  (let* ((alice (with-vat a-vat (spawn ^greeter "Alice")))
         (result
          (resolve-vow-and-return-result
           b-vat
           (lambda ()
             (all-of*
              (map (lambda (i) (<- alice (format #f "Bob~a" i)))
                   (iota 10)))))))
    (match result
      (#('ok val) val)
      (#('err err) (list '*error* err)))))


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

;; Test for https://codeberg.org/spritely/goblins/issues/101
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

(test-assert "Filtering event tree keeps only peers that satisfy predicate"
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

;; Persistence.
;; (define* (^incrementer bcom #:optional [value 0])
;;   (define (main-beh)
;;     (bcom (^incrementer bcom (+ 1 value)) (+ 1 value)))
;;   (define (self-portrait)
;;     (list value))
;;   (portraitize main-beh self-portrait))

;; (define* (^persistent-greeter bcom our-name #:optional [init-number-of-times #f])
;;   (define number-of-times
;;     (or init-number-of-times (spawn ^incrementer)))
;;   (define (main-beh your-name)
;;     (format #f "Hello ~a, my name is ~a (called ~a)."
;;             your-name our-name ($ number-of-times)))
;;   (define (self-portrait)
;;     (list our-name number-of-times))
;;   (portraitize main-beh self-portrait))

;; (define incrementer-env
;;   (make-persistence-env
;;    (list (make-object-spec '((tests test-core) ^incrementer) ^incrementer))
;;    (list)))
;; (define greeter-env
;;   (make-persistence-env
;;    (list (make-object-spec '((tests test-core) ^persistent-greeter) ^persistent-greeter))
;;    (list incrementer-env)))

;; (define memory-store
;;   (make-memory-store))

;; (define-values (persistent-vat alice bob)
;;   (spawn-persistent-vat
;;    greeter-env
;;    (lambda ()
;;      (values (spawn ^persistent-greeter "Alice")
;;              (spawn ^persistent-greeter "Bob")))
;;    memory-store))

;; (define read-memory-store
;;   (persistence-store-read-proc memory-store))

;; (define-values (portraits roots)
;;   (read-memory-store))

;; (test-equal "Correct number of portraits exist after spawning persistent vat"
;;   4 ;; 2 persistent-greeters + 2 incremeneters
;;   (hash-count (const #t) portraits))

;; (test-equal "Correct number of roots returned after spawning persistent vat"
;;   2
;;   (length roots))

;; ;; Next lets change greet several times to increment the counter.
;; (with-vat persistent-vat
;;   ($ alice "Carol")
;;   ($ alice "Bob"))

;; (define-values (portraits roots)
;;   (read-memory-store))

;; ;; Now try to spawn a new vat using the same persistence store
;; ;; to check it reads the portraits correctly.
;; ;; NOTE: This isn't actually a good idea (to have multiple vats
;; ;; on the same store, this is just for testing)
;; (define-values (persistent-vat1 alice1 bob1)
;;   (spawn-persistent-vat
;;    greeter-env
;;    (lambda ()
;;      (values (spawn ^persistent-greeter "Alice")
;;              (spawn ^persistent-greeter "Bob")))
;;    memory-store))

;; (test-equal "Restored alice into a new persistent vat greets correctly"
;;   (with-vat persistent-vat1
;;     ($ alice1 "Carol"))
;;   "Hello Carol, my name is Alice (called 3).")

;; (test-equal "Restored bob into a new persistent vat greets correctly"
;;   (with-vat persistent-vat1
;;     ($ bob1 "Carol"))
;;   "Hello Carol, my name is Bob (called 1).")
  
;; Test persisting an actor which introduces new actors
;; not previously in the graph.
(define-actor (^list bcom #:optional [items '()])
  (lambda (obj)
    (bcom (^list bcom (cons obj items)))))

(define list-env
  (make-persistence-env
   (list (list '((tests test-vat) ^list) ^list))))

(define memory-store
  (make-memory-store))
(define read-from-store
  (persistence-store-read-proc memory-store))

(define-values (persistent-vat list1 list2)
  (spawn-persistent-vat
   list-env
   (lambda ()
     (values (spawn ^list)
             (spawn ^list)))
   memory-store
   #:version 72))

(define one
  (with-vat persistent-vat
    (spawn ^list)))
(define two
  (with-vat persistent-vat
    (spawn ^list)))

(with-vat persistent-vat
  ($ list1 one)
  ($ list1 two)
  ($ list2 one))

(define-values (vat-aurie-id roots-version portraits _roots)
  (read-from-store 'graph-and-slots))
  
(test-equal "Check the version is saved properly"
  72
  roots-version)

;; There should be 4 objs: one, two, list1, list2
(test-equal "Number of objects portraits is correct amount"
  4
  (hash-count (const #t) portraits))

;; Now add two to list2 (not adding any new objects to the graph)
(with-vat persistent-vat
  ($ list2 two))
(define-values (vat-aurie-id _roots-version portraits _roots)
  (read-from-store 'graph-and-slots))
(test-equal "Number of objects in graph remains same when no new object introduced"
  4
  (hash-count (const #t) portraits))

;; Add a new object to the graph by adding it to one of the
;; existing children.
(with-vat persistent-vat
  ($ one (spawn ^list)))
(define-values (vat-aurie-id _roots-version portraits _roots)
  (read-from-store 'graph-and-slots))

(test-equal "Number of objects in graph increases when new object added to child"
  5
  (hash-count (const #t) portraits))

(with-vat persistent-vat
  ($ list2 (spawn ^list)))
(define-values (vat-aurie-id _roots-version portraits _roots)
  (read-from-store 'graph-and-slots))

(test-equal "Number of objects in graph increases when new object added to parent"
  6
  (hash-count (const #t) portraits))

(test-equal "call-system-op-with-vat gets vat"
  (list 'got-vat a-vat)
  (call-system-op-with-vat
   a-vat (lambda (vat) (list 'got-vat vat))))

;; Test changing the behavior
(define-actor (^foo _bcom)
  (lambda ()
    'foo))

(define foo-env
  (make-persistence-env
   `((((tests test-vat) ^foo) ,^foo))))

(define foo-mem (make-memory-store))
(define-values (persistent-vat foo)
  (spawn-persistent-vat
   foo-env
   (lambda ()
     (spawn ^foo))
   foo-mem))

(define-actor (^foo _bcom)
  (lambda ()
    'bar))

(vat-replace-behavior! persistent-vat)
(test-equal "Object behavior changes after using vat-replace-behavior!"
  'bar
  (with-vat persistent-vat
    ($ foo)))

;; Now change the behavior but add an object to the env
(define-actor (^bar _bcom)
  (lambda ()
    'i-am-bar))
(define-actor (^foo _bcom bar)
  (lambda ()
    bar))
(define (restore-foo _version)
  (spawn ^foo (spawn ^bar)))
(define foo-env
  (make-persistence-env
   `((((tests test-vat) ^foo) ,^foo ,restore-foo)
     (((tests test-vat) ^bar) ,^bar))))
(vat-replace-behavior! persistent-vat foo-env)

(test-assert
    (live-refr?
     (with-vat persistent-vat
       ($ foo))))
(test-equal "Objects can add new objects when using vat-replace-behavior!"
  'i-am-bar
  (with-vat persistent-vat
    ($ ($ foo))))

(define aurie-vat (spawn-vat))
(define persistence-registry
  (with-vat aurie-vat
    (spawn ^persistence-registry)))

(define a-vat-store (make-memory-store))
(define-values (a-vat a-cell)
  (spawn-persistent-vat
   cell-env
   (lambda () (spawn ^cell))
   a-vat-store
   #:persistence-registry persistence-registry))

(define b-vat-store (make-memory-store))
(define-values (b-vat b-cell)
  (spawn-persistent-vat
   cell-env
   (lambda () (spawn ^cell (list a-cell a-cell)))
   b-vat-store
   #:persistence-registry persistence-registry))
(vat-halt! a-vat)
(vat-halt! b-vat)

;; Now restore from the same memory stores using an aurie registry
(define persistence-registry*
  (with-vat aurie-vat
    (spawn ^persistence-registry)))

(define-values (a-vat* a-cell*)
  (spawn-persistent-vat
   cell-env
   (lambda () (error "Should be being restored from the memory"))
   a-vat-store
   #:persistence-registry persistence-registry*))

(define-values (b-vat* b-cell*)
  (spawn-persistent-vat
   cell-env
   (lambda () (error "Should be being restored from the memory"))
   b-vat-store
   #:persistence-registry persistence-registry*))

(test-assert "A far reference can be persisted and restored"
  (match (resolve-vow-and-return-result
          b-vat*
          (lambda ()
            (on (<- b-cell*)
                all-of*
                #:promise? #t)))
    [#(ok (hopefully-far-refr1 hopefully-far-refr2))
     (and (with-vat b-vat* (far-refr? hopefully-far-refr1))
          (eq? hopefully-far-refr1 a-cell*)
          (eq? hopefully-far-refr1 hopefully-far-refr2))]))
(vat-halt! a-vat*)
(vat-halt! b-vat*)

;; Far refrs are restored as promises which asynchronously wait on the aurie
;; registry. This is so that vats aren't dependent on each other when restoring.
;; When a object with a far refr persists before the promises resolve we
;; shouldn't loose the far refr info we knew at resturation.
(define persistence-registry
  (with-vat aurie-vat
    (spawn ^persistence-registry)))
(define-values (b-vat* b-cell*)
  (spawn-persistent-vat
   cell-env
   (lambda () (error "Should be being restored from the memory"))
   b-vat-store
   #:persistence-registry persistence-registry))
(define b-store-read-proc (persistence-store-read-proc b-vat-store))
(define b-cell-aurie-id (local-object-refr-aurie-id b-cell*))
(define b-cell-portrait (b-store-read-proc 'object-portrait b-cell-aurie-id))

;; b-cell contains a list with far-refrs to a-cell (a-cell a-cell).
;; Set the same contents to b-cell as it has now to cause it to `bcom` and thus
;; persist. We can then check to see it's portrait is the same.
(with-vat b-vat*
  ($ b-cell* ($ b-cell*)))

(test-equal "Aurie doesn't loose information when restoring far refrs"
  b-cell-portrait
  (b-store-read-proc 'object-portrait b-cell-aurie-id))
(vat-halt! b-vat*)

;; Test upgrading the roots of a vat
(define memory (make-memory-store))
(define-values (vat a-cell)
  (spawn-persistent-vat
   cell-env
   (lambda ()
     (spawn ^cell))
   memory))

;; Stop the vat as we're done with it.
(vat-halt! vat)

(let ((found-cell #f)
      (new-root-one #f)
      (new-root-two #f))
  (define-values (vat* a-cell b-cell)
    (spawn-persistent-vat
     cell-env
     (lambda ()
       (error "Should not be spawning fresh roots"))
     memory
     #:version 1
     #:upgrade
     (migrations
      [(1 cell)
       (set! found-cell cell)
       (set! new-root-one (spawn ^cell))
       (set! new-root-two (spawn ^cell))
       ;; Actually do the upgrade
       (list new-root-one new-root-two)])))
    (test-assert "Previous version was a live-refr"
      (live-refr? found-cell))
    (test-equal "Got upgraded two cells as values"
      (list new-root-one new-root-two)
      (list a-cell b-cell)))
;; Test sending messages to a far refr in the constructor when we have
;; persistence enabled with the registry. Then check we can restore.
(define far-vat (spawn-vat))
(define far-refr
  (with-vat far-vat
    (spawn ^cell 'yay)))

(define-actor (^send-far-refr _bcom)
  (define cell-value-vow (<- far-refr))
  (lambda () cell-value-vow))

(define aurie-far-refr-env
  (namespace-env (tests goblins vat aurie-far-refr) ^send-far-refr))

(define far-refr-memory (make-memory-store))
(define-values (aurie-vat send-far-refr)
  (spawn-persistent-vat
   aurie-far-refr-env
   (lambda ()
     (spawn ^send-far-refr))
   far-refr-memory
   ;; It's not important what the registry is, we just want to go through the code
   ;; path which handles restoring vats with far refrs.
   #:persistence-registry persistence-registry))

(vat-halt! aurie-vat)

(define-values (aurie-vat* send-far-refr*)
  (spawn-persistent-vat
   aurie-far-refr-env
   (lambda ()
     (spawn ^send-far-refr))
   far-refr-memory
   ;; See comment above.
   #:persistence-registry persistence-registry))

(test-equal "Restoring refr which sends message to far refr on construction in vat with aurie registry"
  #(ok yay)
  (resolve-vow-and-return-result
   aurie-vat*
   (lambda () (<- send-far-refr*))))

;; Test that objects not in roots persist after resturation
(define memory (make-memory-store))
(define-values (aurie-vat0 cell0)
  (spawn-persistent-vat
   cell-env
   (lambda ()
     (spawn ^cell (spawn ^cell 0)))
   memory))

(vat-halt! aurie-vat0)
(define-values (aurie-vat1 cell1)
  (spawn-persistent-vat
   cell-env
   (lambda ()
     (error "Should be from memory"))
   memory))

(with-vat aurie-vat1
  (define inner-cell ($ cell1))
  ($ inner-cell 1))

(vat-halt! aurie-vat1)

(define-values (aurie-vat2 cell2)
  (spawn-persistent-vat
   cell-env
   (lambda ()
     (error "Should be from memory"))
   memory))

(test-equal "Restored non-root objects are persisted when modified"
  #(ok 1)
  (resolve-vow-and-return-result
   aurie-vat2
   (lambda () (<- (<- cell2)))))

;; Check with-vat doesn't lockup the vat if called within a vat
(test-assert "Check nested with-vat returns promise, and doesn't lockup the vat"
  (let ((vat (spawn-vat)))
    (promise-refr?
     (with-vat vat
       (with-vat vat
         'hello)))))

;; Check <-hashmap-ref, <-list-ref and <-tagged-ref work across vats
(define vat1
  (spawn-vat))
(define vat2
  (spawn-vat))

(define (^hashmap bcom)
  (lambda ()
    (hashmap ("foo" 'bar))))
(define hm-actor
  (with-vat vat1
    (spawn ^hashmap)))

(test-equal "<-hashmap-ref works across vats"
  (resolve-vow-and-return-result
   vat2
   (lambda ()
     (define hm-vow (<- hm-actor))
     (<-hashmap-ref hm-vow "foo")))
  #(ok bar))

(define (^list bcom)
  (lambda ()
    '(foo bar baz)))
(define list-actor
  (with-vat vat1
    (spawn ^list)))

(test-equal "<-list-ref works across vats"
  (resolve-vow-and-return-result
   vat2
   (lambda ()
     (define lst-vow (<- list-actor))
     (<-list-ref lst-vow 0)))
  #(ok foo))

(define (^tagged bcom)
  (lambda ()
    (make-tagged "hello" 'beepboop)))
(define tagged-actor
  (with-vat vat1
    (spawn ^tagged)))

(test-equal "<-tagged-ref works across vats"
  (resolve-vow-and-return-result
   vat2
   (lambda ()
     (define tagged-vow (<- tagged-actor))
     (<-tagged-ref tagged-vow "hello")))
  #(ok beepboop))

(test-end "test-vat")
