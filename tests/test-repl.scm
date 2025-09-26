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

(define-module (goblins test-repl)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-64)
  #:use-module (tests repl-driver))

(test-begin "test-repl")

(define-syntax-rule (test-repl description regexp commands ...)
  (test-assert description
    (call-with-repl-driver
     (lambda (driver)
       (repl-driver-meta driver '(import (goblins)
                                         (fibers conditions)
                                         (fibers operations)))
       (let ((output (repl-driver-run driver '(commands ...))))
         (format #t "REPL output:\n~a\n" output);
         (string-match regexp output))))))

(test-repl ",vats with no vats"
           "No vats"
           (meta vats))

(test-repl ",vats with a vat"
           "  0\trunning\t  [✗?] \t    0\talice"
           (eval (define a-vat (spawn-vat #:name 'alice)))
           (meta vats))

(test-repl ",enter-vat using an id"
           "Entering vat '0'"
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat 0))

(test-repl ",enter-vat using a vat object"
           "Entering vat '0'"
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat a-vat))

(test-repl ",enter-vat informs the user when argument is not a vat"
           "Not a vat"
           (meta enter-vat 'foo))

(test-repl ",vat-log-enable doesn't work outside of Goblins REPL"
           "Not in a vat"
           (meta vat-log-enable))

(test-repl ",vat-log-enable works inside of Goblins REPL"
           "Logging enabled"
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat a-vat)
           (meta vat-log-enable))

(test-repl ",vat-log-disable doesn't work outside of Goblins REPL"
           "Not in a vat"
           (meta vat-log-disable))

(test-repl ",vat-log-disable works inside of Goblins REPL"
           "Logging disabled"
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat a-vat)
           (meta vat-log-disable))

(test-repl ",vat-tail doesn't work outside of Goblins REPL"
           "Not in a vat"
           (meta vat-tail))

(test-repl ",vat-tail displays a warning when logging is disabled"
           "warning: Logging is disabled"
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat a-vat)
           (meta vat-tail))

(test-repl ",vat-tail displays a list of events when logging is enabled"
           "\\(receive message #<local-object \\^call-with-vat>\\)"
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat a-vat)
           (meta vat-log-enable)
           (eval '(+ 1 2 3))
           (meta vat-tail))

(test-repl ",vat-trace doesn't work outside of Gobins REPL"
           "Not in a vat"
           (meta vat-trace))

(test-repl ",vat-trace displays a warning when logging is disabled"
           "warning: Logging is disabled"
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat a-vat)
           (meta vat-trace))

(test-repl ",vat-trace displays a vat event backtrace when logging is enabled"
           "In vat alice:
  Churn 1:
    1: \\(message #<local-object \\^call-with-vat>\\)"
           (eval (define a-vat (spawn-vat #:name 'alice)))
           (meta enter-vat a-vat)
           (meta vat-log-enable)
           (eval '(+ 1 2 3))
           (meta vat-trace))

(test-repl ",vat-tree doesn't work outside of Goblins REPL"
           "Not in a vat"
           (meta vat-tree))

(test-repl ",vat-tree displays a warning when logging is disabled"
           "warning: Logging is disabled"
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat a-vat)
           (meta vat-tree))

(test-repl ",vat-tree displays a vat event tree when logging is enabled"
           "Vat alice, [0-9]+: \\(message #<local-object \\^call-with-vat>\\)
.. Vat bob, [0-9]+: \\(message #<local-object>\\)
   .. Vat alice, [0-9]+: \\(message #<local-object \\resolver> fulfill \"hello\"\\)"
           (eval (define a-vat (spawn-vat #:name 'alice)))
           (eval (define b-vat (spawn-vat #:name 'bob)))
           (meta enter-vat b-vat)
           (meta vat-log-enable)
           (eval (define bob
                   (spawn (lambda (_bcom)
                            (lambda () "hello")))))
           (meta quit)
           (meta enter-vat a-vat)
           (meta vat-log-enable)
           (eval (define bob-messaged (make-condition)))
           (meta vat-resolve
                 (on (<- bob)
                     (lambda (response)
                       (format #f "Bob said: ~a" response)
                       (signal-condition! bob-messaged))))
           (meta quit)
           (eval
            (perform-operation
             (choice-operation (wrap-operation (sleep-operation 2)
                                               (lambda _ (error 'timeout)))
                               (wait-operation bob-messaged))))
           (meta enter-vat a-vat)
           (meta vat-tree))

(test-repl ",vat-errors doesn't work outside of Goblins REPL"
           "Not in a vat"
           (meta vat-errors))

(test-repl ",vat-errors displays a special message when there are no logged errors"
           "No errors"
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat a-vat)
           (meta vat-errors))

(test-repl ",vat-errors displays a list of errors when there are logged errors"
           "At churn 1, event 1, file .+:[0-9]+:[0-9]+:
  In procedure raise-exception: Wrong type argument in position 1: \"two\""
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat a-vat)
           (meta vat-log-enable)
           (eval (+ 1 "two"))
           (meta quit)
           (meta vat-errors))

(test-repl ",vat-peek uses actormap snapshots while debugging"
           "gold"
           (meta import (goblins actor-lib cell) (goblins vat))
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat a-vat)
           (meta vat-log-enable)
           (eval (define chest (spawn ^cell 'gold)))
           ;; Create a multi-step process where the cell is updated
           ;; and then an error happens.
           (meta vat-resolve
                 (on (<- chest 'sword)
                     (lambda _ (error "oh no"))))
           ;; Debug the error that just happened out-of-band.
           (meta vat-debug (- (vat-clock a-vat) 1))
           ;; Move up the trace 4 events to get to a step where the
           ;; cell had its original value.
           (meta vat-up)
           (meta vat-up)
           (meta vat-up)
           (meta vat-up)
           (meta vat-peek chest))

(test-repl ",vat-up doesn't allow moving past the oldest event in the trace"
           "Already at oldest event"
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat a-vat)
           (meta vat-log-enable)
           (eval (error "oh no"))
           (meta vat-up))

(test-repl ",vat-down doesn't allow moving past the newest event in the trace"
           "Already at most recent event"
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat a-vat)
           (meta vat-log-enable)
           (eval (error "oh no"))
           (meta vat-down))

(test-repl ",vat-debug can debug events that did not raise an exception"
           "sword"
           (meta import (goblins actor-lib cell))
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat a-vat)
           (meta vat-log-enable)
           (eval (define cell (spawn ^cell 'gold)))
           (eval ($ cell 'sword))
           (eval ($ cell 'shield))
           (meta vat-debug 3)
           (meta vat-peek cell))

(test-repl ",vat-resolve prints result when promise is fulfilled"
           "Alice"
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat a-vat)
           (eval (define alice (spawn (lambda _bcom (lambda () "Alice")))))
           (meta vat-resolve (<- alice)))

(test-repl ",vat-resolve prints exception when promise is broken"
           "Promise broken"
           (eval (define a-vat (spawn-vat)))
           (meta enter-vat a-vat)
           (eval (define broken
                   (spawn (lambda _bcom (lambda () (+ 1 "two"))))))
           (meta vat-resolve (<- broken)))

(test-repl ",vat-replace-behavior! upgrades behavior"
           "bar"
           (meta import (goblins persistence-store memory))
           (eval (define-actor (^foo _bcom) (lambda () 'foo)))
           (eval (define env (make-persistence-env `((foo ,^foo)))))
           (eval (define-values (a-vat foo)
                   (spawn-persistent-vat env (lambda () (spawn ^foo)) (make-memory-store))))
           (eval (define-actor (^foo _bcom) (lambda () 'bar)))
           (meta enter-vat a-vat)
           (meta vat-replace-behavior)
           (eval ($ foo)))

(test-repl ",vat-replace-behavior! upgrades behavior with new persistence env"
           "i-am-bar"
           (meta import (goblins persistence-store memory))
           (eval (define-actor (^foo _bcom) (lambda () 'foo)))
           (eval (define env (make-persistence-env `((foo ,^foo)))))
           (eval (define-values (a-vat foo)
                   (spawn-persistent-vat env (lambda () (spawn ^foo)) (make-memory-store))))
           (eval (define-actor (^bar _bcom) (lambda () 'i-am-bar)))
           (eval (define-actor (^foo _bcom bar) (lambda () bar)))
           (eval (define (restore-foo _version) (spawn ^foo (spawn ^bar))))
           (eval (define new-env
                   (make-persistence-env `((foo ,^foo ,restore-foo) (bar ,^bar)))))
           (meta enter-vat a-vat)
           (meta vat-replace-behavior new-env)
           (eval ($ ($ foo))))

(test-repl ",vat-log-resize changes the size of the vat's log"
           "Size is: 2"
           (meta import (goblins vat))
           (eval (define vat (spawn-vat)))
           (eval (vat-log-resize! vat 100))
           (meta enter-vat vat)
           (meta vat-log-resize 2)
           (eval (format #f "Size is: ~a" (vat-log-capacity vat))))

(test-end "test-repl")
