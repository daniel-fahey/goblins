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

(use-modules (goblins)
             (goblins actor-lib sealers)
	     (tests utils)
	     (ice-9 match)
             (srfi srfi-64))

(test-begin "test-sealers")

(define am (make-actormap))

(define-values (alice-sealer alice-unsealer alice-sealed?)
  (actormap-churn-run! am spawn-sealer-triplet))
(define-values (bob-sealer bob-unsealer bob-sealed?)
  (actormap-run! am spawn-sealer-triplet))

(define alice-sealed-lunch
  (actormap-churn-run! am (lambda () (<- alice-sealer 'chickpea-salad))))
(define bob-sealed-lunch
  (actormap-churn-run! am (lambda () (<- bob-sealer 'bbq-lentils))))

(test-equal "Alice can unseal her own lunch"
  #(ok chickpea-salad)
  (am-resolve-vow-and-return-result
   am
   (lambda ()
     (<- alice-unsealer alice-sealed-lunch))))

(test-equal
    "Alice's lunch confirms it's sealed with her sealed? trademark"
  #(ok #t)
  (am-resolve-vow-and-return-result
   am
   (lambda ()
     (<- alice-sealed? alice-sealed-lunch))))


(test-equal "Bob can unseal his own lunch"
  #(ok bbq-lentils)
  (am-resolve-vow-and-return-result
   am
   (lambda ()
     (<- bob-unsealer bob-sealed-lunch))))

(test-equal
    "Bob's lunch confirms it's sealed with his sealed? trademark"
  #(ok #t)
  (am-resolve-vow-and-return-result
   am
   (lambda ()
     (<- bob-sealed? bob-sealed-lunch))))

(test-equal
    "Bob's trademark doesn't claim to have sealed alice's lunch"
  #(ok #f)
  (am-resolve-vow-and-return-result
   am
   (lambda ()
     (<- bob-sealed? alice-sealed-lunch))))

(test-equal
    "Alice's trademark doesn't claim to have sealed bob's lunch"
  #(ok #f)
  (am-resolve-vow-and-return-result
   am
   (lambda ()
     (<- alice-sealed? bob-sealed-lunch))))

(test-assert
    "Alice can't unseal bob's lunch"
  (match (am-resolve-vow-and-return-result
	  am
	  (lambda ()
	    (<- alice-unsealer bob-sealed-lunch)))
    [#(err _err) #t]
    [_ #f]))

(test-assert
 "Bob can't unseal alice's lunch"
 (match (am-resolve-vow-and-return-result
	 am
	 (lambda ()
	   (<- bob-unsealer alice-sealed-lunch)))
   [#(err _err) #t]
   [_ #f]))

;; Custom sealer triplet
(define custom-sealer-name #f)
(define custom-sealer-sealed-value #f)
(define* (make-sealer-triplet #:optional name)
  (set! custom-sealer-name name)
  (define (seal value)
    (set! custom-sealer-sealed-value value)
    `(sealed ,value))
  (define (unseal sealed-value)
    (if (eq? (car sealed-value) 'sealed)
        (cadr sealed-value)))
  (define (sealed? maybe-sealed)
    (and (list? maybe-sealed)
         (eq? (car maybe-sealed) 'sealed)))
  (values seal unseal sealed?))

(define known-sealers
  (acons '((tests actor-lib test-sealers) make-sealer-triplet)
	 make-sealer-triplet
	 default-sealers-alist))

(define-values (custom-spawn-sealer-triplet custom-sealers-env)
  (make-spawn-sealer-triplet '(tests actor-lib test-sealers) known-sealers))

(define-values (carol-sealer carol-unsealer carol-sealed?)
  (actormap-churn-run!
   am
   (lambda ()
     (custom-spawn-sealer-triplet 'carol-sealer-triplet
                            #:make-sealer-triplet make-sealer-triplet))))

(define carol-sealed-lunch
  (actormap-churn-run! am (lambda () (<- carol-sealer 'tofu-scramble))))

(test-equal "Check setting the name of a sealer triplet"
  'carol-sealer-triplet
  custom-sealer-name)

(test-equal "Check carol can unseal her lunch"
  #(ok tofu-scramble)
  (am-resolve-vow-and-return-result
   am
   (lambda ()
     (<- carol-unsealer carol-sealed-lunch))))

(test-equal
    "Carol's lunch confirms it's sealed with her sealed? trademark"
  #(ok #t)
  (am-resolve-vow-and-return-result
   am
   (lambda ()
     (<- carol-sealed? carol-sealed-lunch))))

(test-equal "Carol's sealed lunch uses custom sealers"
  'tofu-scramble
  custom-sealer-sealed-value)

(test-end "test-sealers")
