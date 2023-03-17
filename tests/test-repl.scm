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
       (repl-driver-meta driver '(import (goblins)))
       (let ((output (repl-driver-run driver '(commands ...))))
         (string-match regexp output))))))

(test-repl ",vats with no vats"
           "No vats"
           (meta vats))

(test-repl ",vats with a vat"
           "0\trunning\tdisabled\talice"
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
           "\\(receive #<local-object>\\)"
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
    1: \\(receive #<local-object>\\)"
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
           "Vat alice, 1: \\(receive #<local-object>\\)
Vat alice, 2: \\(receive listen #<local-promise>\\)
Vat alice, 3: \\(send #<local-object>\\)
... Vat bob, 4: \\(receive #<local-object>\\)
... Vat bob, 5: \\(send #<local-object \\^resolver> fulfill \"hello\"\\)
    ... Vat alice, 6: \\(receive #<local-object \\^resolver> fulfill \"hello\"\\)
    ... Vat alice, 7: \\(receive #<local-object \\^on-listener> fulfill \"hello\"\\)
    ... Vat alice, 8: \\(receive #<local-object fulfilled-handler> \"hello\"\\)"
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
           (eval (on (<- bob)
                     (lambda (response)
                       (format #f "Bob said: ~a" response))))
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
           (eval (on (<- chest 'sword)
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

(test-end "test-repl")
