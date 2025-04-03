;;; Copyright 2020-2021 Christine Lemmer-Webber
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

(define-module (tests actor-lib on)
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module (goblins)
  #:use-module (goblins actor-lib on)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils))

(test-begin "test-on")

(define a-vat (spawn-vat))

(define (^doubler _bcom)
  (lambda (x)
    (* x 2)))

(define doubler (with-vat a-vat (spawn ^doubler)))

(test-assert "let-on returns a promise"
  (promise-refr?
   (with-vat a-vat
     (let-on ((four (<- doubler 2)))
       #t))))

(test-equal "let-on supports multiple bindings"
  #(ok 12)
  (resolve-vow-and-return-result
   a-vat
   (lambda ()
     (let-on ((four (<- doubler 2))
              (eight (<- doubler 4)))
       (+ four eight)))))

(test-assert "let*-on returns a promise"
  (promise-refr?
   (with-vat a-vat
     (let*-on ((four (<- doubler 2)))
       #t))))

(test-equal "let*-on supports multiple sequential bindings"
  #(ok 12)
  (resolve-vow-and-return-result
   a-vat
   (lambda ()
     (let*-on ((four (<- doubler 2))
               (eight (<- doubler four)))
       (+ four eight)))))

(test-equal "on-each will iterater over each item the vow resolves to"
  #(ok (3 2 1))
  (resolve-vow-and-return-result
   a-vat
   (lambda ()
     (define-values (vow resolver)
       (spawn-promise-and-resolver))
     ($ resolver 'fulfill '(1 2 3))
     (define-values (done-vow done-resolver)
       (spawn-promise-and-resolver))
     (let ((result '()))
       (on-each
        (lambda (value)
          (set! result (cons value result))
          (when (equal? value 3)
            ($ done-resolver 'fulfill result)))
        vow))
     done-vow)))

(test-equal "on-map iterates over each item the vow resolves to returning a promise"
  #(ok (2 4 6))
  (resolve-vow-and-return-result
   a-vat
   (lambda ()
     (define-values (vow resolver)
       (spawn-promise-and-resolver))
     ($ resolver 'fulfill '(1 2 3))
     (define (double x) (* x 2))
     (on-map double vow))))

(test-equal "on-filter iterates over each item in vow, filtering returning a promise"
  #(ok (2 4 6 8 10))
  (resolve-vow-and-return-result
   a-vat
   (lambda ()
     (define-values (vow resolver)
       (spawn-promise-and-resolver))
     ($ resolver 'fulfill '(1 2 3 4 5 6 7 8 9 10))
     (on-filter even? vow))))

(test-equal "on-match matches on provided clauses and returns promise of result"
  #(ok two)
  (resolve-vow-and-return-result
   a-vat
   (lambda ()
     (define-values (vow resolver)
       (spawn-promise-and-resolver))
     ($ resolver 'fulfill '(result two))
     (on-match vow
       (('error result) (error "oh no"))
       (('result 'one) 1)
       (('result something-else) something-else)))))

(test-end "test-on")
