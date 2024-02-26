;;; Copyright 2021 Christine Lemmer-Webber
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

(define-module (tests test-await)
  #:use-module (goblins core)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(test-begin "test-await")

(define am (make-actormap))

(define (^greeter _bcom my-name)
  (lambda (your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name)))

(define (^blows-up _bcom)
  (lambda (your-name)
    (error "kaboom!")))

(define alice1-sees #f)
(define alice2-sees #f)
(define caught-error? #f)
(define flipped-forbidden? #f)

(parameterize ((current-error-port (%make-void-port "w")))
  (actormap-churn-run!
   (make-actormap)
   (lambda ()
     (define (^greeter _bcom my-name)
       (lambda (your-name)
         (format #f "Hello ~a, my name is ~a!" your-name my-name)))
     (define (^exploder _bcom)
       (lambda ()
         (error "ka-boom!!!")))
     (define bob (spawn ^greeter "Bob"))
     (define exploder (spawn ^exploder))
     (set! alice1-sees (format #f "First: ~a\n" (await (<- bob "Alice1"))))
     (set! alice2-sees (format #f "Second: ~a\n" (<<- bob "Alice2")))
     (catch #t
       (lambda ()
         (<<- exploder))
       (lambda _
         (set! caught-error? #t)))
     (<<- exploder)
     (set! flipped-forbidden? #t))
   #:catch-errors? #t))

(test-equal "First: Hello Alice1, my name is Bob!\n" alice1-sees)
(test-equal "Second: Hello Alice2, my name is Bob!\n" alice2-sees)
(test-assert caught-error?)
(test-assert (not flipped-forbidden?))

;; Ensure that synchronous calls to actors who await don't suspend parent
(let* ((post-synchronous-call-flipped? #f)
       (alice-hears #f)
       (returned-val
        (actormap-churn-run!
         (make-actormap)
         (lambda ()
           (define (^greeter _bcom my-name)
             (lambda (your-name)
               (format #f "Hello ~a, my name is ~a!" your-name my-name)))
           (define (^calls-greeter _bcom my-name)
             (lambda (greeter)
               (set! alice-hears
                     (format #f "I heard back: ~a\n"
                             (<<- greeter my-name)))))
           (define bob (spawn ^greeter "Bob"))
           (define alice (spawn ^calls-greeter "Alice"))
           (test-equal ($ alice bob) '*awaited*)
           (set! post-synchronous-call-flipped? #t)
           'what-we-return)
         #:catch-errors? #t)))
  (test-equal "I heard back: Hello Alice, my name is Bob!\n" alice-hears)
  (test-assert post-synchronous-call-flipped?)
  (test-equal 'what-we-return returned-val))

(test-end "test-await")
