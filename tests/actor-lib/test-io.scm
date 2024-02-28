;;; Copyright 2024 Christine Lemmer-Webber
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

(define-module (tests actor-lib io)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib io)
  #:use-module (goblins actor-lib joiners)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (tests utils)
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module (fibers timers))

(test-begin "test-io")

(define a-vat (spawn-vat #:name 'a))

(test-equal `(#\e #\l #\l #\o #\space #\V #\i #\v #\i #\! ,the-eof-object)
  (let* ((ip (open-input-string "Hello Vivi!"))
         (ip-actor (with-vat a-vat
                     (spawn ^io ip
                            ;; burn one character off the top
                            #:init get-char)))
         (resolve-next-char
          (lambda ()
            (define result
              (resolve-vow-and-return-result
               a-vat
               (lambda ()
                 (<- ip-actor get-char))))
            (match result
              (#('ok val) val)
              (#('err err) (list '*error* err))))))
    (map (lambda _ (resolve-next-char))
         (iota 11))))

(let* ((halted? (make-condition))
       (ip (open-input-string "Hello Vivi!"))
       (ip-actor (with-vat a-vat
                   (spawn ^io ip
                          ;; burn one character off the top
                          #:init get-char
                          #:cleanup (lambda (ip)
                                      (signal-condition! halted?))))))
  (with-vat a-vat ($ ip-actor 'halt))
  (test-assert "IO actor performs cleanup"
    (perform-operation
     (choice-operation (wrap-operation (sleep-operation 2)
                                       (lambda _ #f))
                       (wrap-operation (wait-operation halted?)
                                       (lambda _ #t)))))
  (test-error "Halted IO actor moves to defunct behavior"
              #t (with-vat a-vat ($ ip-actor get-char))))

(test-end "test-io")
