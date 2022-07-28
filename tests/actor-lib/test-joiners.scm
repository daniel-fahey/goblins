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

(define-module (tests actor-lib test-joiners)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib joiners)
  #:use-module (srfi srfi-64))

(test-begin "test-joiners")

(define a-vat
  (spawn-vat))

(define (^double-evens _bcom)
  (lambda (val)
    (if (even? (pk 'val val))
        (* val 2)
        (error "Freaking out about non-even number! (as expected)"))))

(define double-evens
  (a-vat (lambda () (spawn  ^double-evens))))

(define (run-joiner-get-result joiner . nums)
  (define result #f)
  (a-vat
   (lambda ()
     (define evens-vows
       (map (lambda (num) (<- double-evens num)) nums))
     (on (apply joiner evens-vows)
         (lambda (val)
           (set! result `(fulfilled ,val)))
         #:catch
         (lambda (err)
           (set! result `(broken ,err))))))
  (sleep 1)
  result)

(test-equal
    "all-of fulfills promise with list of promise resolutions in case of all succeeding"
  (run-joiner-get-result all-of 2 4 6 8)
  '(fulfilled (4 8 12 16)))

(test-assert
    "all-of breaks promise with first error that is raised"
  (equal? (car (run-joiner-get-result all-of 2 4 7 8))
          'broken))

(test-end "test-joiners")
