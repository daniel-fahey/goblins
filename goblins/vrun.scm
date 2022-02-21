;;; Copyright 2022 Christine Lemmer-Webber
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

(define-module (goblins vrun)
  #:use-module (system repl common)
  #:use-module (system repl command)
  #:use-module (system repl error-handling)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (ice-9 control)
  #:use-module (ice-9 threads)
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (fibers channels)
  #:use-module (fibers nameset)
  #:export (spawn-vat*))

(define (spawn-vat*)
  (let* ((result-ch (make-channel))
         (vat-halt? (make-condition))
         (repl-thread
          (call-with-new-thread
           (lambda ()
             (run-fibers
              (lambda ()
                (define a-vat (spawn-vat))
                (put-message result-ch a-vat)
                (wait vat-halt?))))))
         (repl-vat  ; vat controller procedure
          (get-message result-ch)))
    repl-vat))

(define current-repl-vat (make-parameter #f))

(define (ensure-current-repl-vat)
  (or (current-repl-vat)
      (let ((new-vat (spawn-vat*)))
        (current-repl-vat new-vat)
        new-vat)))

(define-syntax-rule (vr body ...)
  ((ensure-current-repl-vat) 'run (lambda () body ...)))

(define (vr-form-transform form)
  (syntax-case form (define define-values)
    ((define id exp)
     #'(define id
         ((ensure-current-repl-vat) 'run (lambda () exp))))
    ((define-values (ids ...) exp)
     #'(define-values (ids ...)
         ((ensure-current-repl-vat) 'run (lambda () exp))))
    (exp
     #'((ensure-current-repl-vat) 'run (lambda () exp)))))

;; borrowed from guile/system/repl/repl.scm
(define (with-stack-and-prompt thunk)
  (call-with-prompt (default-prompt-tag)
    (lambda () (start-stack #t (thunk)))
    (lambda (k proc)
      (with-stack-and-prompt (lambda () (proc k))))))

;; this too
(define-syntax-rule (abort-on-error string exp)
  (catch #t
    (lambda () exp)
    (lambda (key . args)
      (format #t "While ~A:~%" string)
      (print-exception (current-output-port) #f key args)
      (abort))))

;; and this munged from that
(define-inlinable (vrun-meta-command repl exp)
  (call-with-values
      (lambda ()
        (repl-eval repl (vr-form-transform exp)))
    (lambda l
      (for-each (lambda (v)
                  (repl-print repl v))
                l))))

(define-meta-command ((vat-run goblins) repl exp)
  "vat-run EXP
Evaluate EXP within a vat churn.

Returns the result of the initial message.
Waits until a new turn is available; if the vat is busy, will block
until it is available.

Doesn't enter a prompt like you might want on exceptions, though we would love
to implement that.

(Aliases: ,vrun ,vr)"
  (vrun-meta-command repl exp))

(define-meta-command ((vrun goblins) repl exp)
  "vrun EXP
Same thing as ,vat-run except with fewer characters."
  (vrun-meta-command repl exp))

(define-meta-command ((vr goblins) repl exp)
  "vr EXP
Same thing as ,vat-run except with fewer characters."
  (vrun-meta-command repl exp))
