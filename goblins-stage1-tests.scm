;;; Copyright 2019-2021 Christine Lemmer-Webber
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

(define-module (goblins tests test-stage1)
  #:use-module (goblins stage1)
  #:use-module (srfi srfi-64))

(test-begin "test-goblins-stage1")

(define am (make-whactormap))

(define (^greeter _bcom my-name)
  (lambda (your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name)))

(define alice
  (actormap-direct-run!
   am
   (lambda ()
     (spawn ^greeter "Alice"))))

(test-equal "Hello Bob, my name is Alice!"
  (actormap-direct-run!
   am
   (lambda ()
     (S alice "Bob"))))

(define (^gregarious _bcom my-name)
  (lambda (talk-to)
    (format #f "I heard back: ~a"
            (S talk-to my-name))))

(define greg
  (actormap-direct-run!
   am
   (lambda ()
     (spawn ^gregarious "Greg"))))

;; Actors which call other actors
(test-equal "I heard back: Hello Greg, my name is Alice!"
  (actormap-direct-run!
   am
   (lambda ()
     (S greg alice))))

;; Actor updates: update and return value separately
(define* (^cell bcom #:optional [val #f])
  (case-lambda
    [() val]
    [(new-val) (bcom (^cell bcom new-val))]))

(define _void (if #f #f))

(actormap-direct-run!
 am
 (lambda ()
   (define cell (spawn ^cell))
   (test-equal (S cell) #f)          ; initial val
   (test-equal (S cell 'foo) _void)  ; update (no return value)
   (test-equal (S cell) 'foo)))      ; new val

;; Actor updates: update and return value at same time
(define* (^counter bcom #:optional [n 0])
  (lambda ()
    (bcom (^counter bcom (1+ n)) n)))

(actormap-direct-run!
 am
 (lambda ()
   (define ctr (spawn ^counter))
   (test-equal 0 (S ctr))
   (test-equal 1 (S ctr))
   (test-equal 2 (S ctr))
   (test-equal 3 (S ctr))))

(test-end "test-goblins-stage1")
