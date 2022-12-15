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
             (srfi srfi-64))

(test-begin "test-sealers")

(define am (make-actormap))

(define-values (alice-sealer alice-unsealer alice-sealed?)
  (actormap-run! am spawn-sealer-triplet))
(define-values (bob-sealer bob-unsealer bob-sealed?)
  (actormap-run! am spawn-sealer-triplet))

(define alice-sealed-lunch
  (actormap-poke! am alice-sealer 'chickpea-salad))
(define bob-sealed-lunch
  (actormap-poke! am bob-sealer 'bbq-lentils))

(test-equal
    "Alice can unseal her own lunch"
  (actormap-peek am alice-unsealer alice-sealed-lunch)
  'chickpea-salad)

(test-assert
    "Alice's lunch confirms it's sealed with her sealed? trademark"
  (actormap-peek am alice-sealed? alice-sealed-lunch))

(test-equal
    "Bob can unseal his own lunch"
  (actormap-peek am bob-unsealer bob-sealed-lunch)
  'bbq-lentils)

(test-assert
    "Bob's lunch confirms it's sealed with his sealed? trademark"
  (actormap-peek am bob-sealed? bob-sealed-lunch))

(test-assert
    "Bob's trademark doesn't claim to have sealed alice's lunch"
  (not (actormap-peek am bob-sealed? alice-sealed-lunch)))

(test-assert
    "Alice's trademark doesn't claim to have sealed bob's lunch"
  (not (actormap-peek am alice-sealed? bob-sealed-lunch)))

(test-error
 "Alice can't unseal bob's lunch"
 (actormap-peek am alice-unsealer bob-sealed-lunch))

(test-error
 "Bob can't unseal alice's lunch"
 (actormap-peek am bob-unsealer alice-sealed-lunch))

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

(define-values (carol-sealer carol-unsealer carol-sealed?)
  (actormap-run!
   am
   (lambda ()
     (spawn-sealer-triplet 'carol-sealer-triplet
                            #:make-sealer-triplet make-sealer-triplet))))

(define carol-sealed-lunch
  (actormap-poke! am carol-sealer 'tofu-scramble))

(test-equal
    "Check setting the name of a sealer triplet"
  custom-sealer-name
  'carol-sealer-triplet)

(test-equal
    "Check carol can unseal her lunch"
  (actormap-peek am carol-unsealer carol-sealed-lunch)
  'tofu-scramble)

(test-assert
    "Carol's lunch confirms it's sealed with her sealed? trademark"
  (actormap-peek am carol-sealed? carol-sealed-lunch))

(test-equal
    "Carol's sealed lunch uses custom sealers"
  custom-sealer-sealed-value
  'tofu-scramble)

(test-end "test-sealers")
