;;; Copyright 2022-2025 Jessica Tallon
;;; Copyright 2024 David Thompson <dave@spritely.institute>
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

(define-module (tests actor-lib test-joiners)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib joiners)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils))

(test-begin "test-joiners")

(define a-vat
  (spawn-vat))

(define (^double-evens _bcom)
  (lambda (val)
    (if (even? val)
        (* val 2)
        (error "Freaking out about non-even number! (as expected)"))))

(define double-evens
  (with-vat a-vat (spawn  ^double-evens)))

(define (run-joiner-get-result joiner . nums)
  (define result #f)
  (with-vat a-vat
   (define evens-vows
     (map (lambda (num) (<- double-evens num)) nums))
   (on (apply joiner evens-vows)
       (lambda (val)
         (set! result `(fulfilled ,val)))
       #:catch
       (lambda (err)
         (set! result `(broken ,err)))))
  (sleep 1)
  result)

(test-equal
    "all-of fulfills promise with list of promise resolutions in case of all succeeding"
  '(fulfilled (4 8 12 16))
  (run-joiner-get-result all-of 2 4 6 8))

(test-assert
    "all-of breaks promise with first error that is raised"
  (equal? (car (run-joiner-get-result all-of 2 4 7 8))
          'broken))

(test-equal "all-of* fulfills promise when passed empty list"
  #(ok ())
  (resolve-vow-and-return-result a-vat (lambda () (all-of* '()))))

(test-equal "race fulfills its own promise if an arg promise is fulfilled first"
  #(ok 42)
  (resolve-vow-and-return-result
   a-vat
   (lambda ()
     (define-values (a-vow a-resolver)
       (spawn-promise-and-resolver))
     (define-values (b-vow b-resolver)
       (spawn-promise-and-resolver))
     (let ((vow (race a-vow b-vow)))
       (<-np a-resolver 'fulfill 42)
       (<-np b-resolver 'break 'uh-oh)
       vow))))

(test-equal "race breaks its own promise if an arg promise is broken first"
  #(err (uh-oh))
  (resolve-vow-and-return-result
   a-vat
   (lambda ()
     (define-values (a-vow a-resolver)
       (spawn-promise-and-resolver))
     (define-values (b-vow b-resolver)
       (spawn-promise-and-resolver))
     (let ((vow (race a-vow b-vow)))
       (<-np a-resolver 'break 'uh-oh)
       (<-np b-resolver 'fulfill 42)
       vow))))

(test-equal "race* fulfills its own promise if an arg promise is fulfilled first"
  #(ok 42)
  (resolve-vow-and-return-result
   a-vat
   (lambda ()
     (define-values (a-vow a-resolver)
       (spawn-promise-and-resolver))
     (define-values (b-vow b-resolver)
       (spawn-promise-and-resolver))
     (let ((vow (race* (list a-vow b-vow))))
       (<-np a-resolver 'fulfill 42)
       (<-np b-resolver 'break 'uh-oh)
       vow))))

(test-end "test-joiners")
