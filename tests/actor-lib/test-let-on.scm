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

(define-module (tests actor-lib simple-mint)
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module (goblins)
  #:use-module (goblins actor-lib let-on)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils))

(test-begin "test-let-on")

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

(test-end "test-let-on")
