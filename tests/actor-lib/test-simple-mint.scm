;;; Copyright 2020 Christine Lemmer-Webber
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
  #:use-module (goblins core)
  #:use-module (goblins actor-lib simple-mint)
  #:use-module (srfi srfi-64))

(test-begin "test-simple-mint")

(define am (make-whactormap))
(define mint (actormap-spawn! am ^mint))

(test-equal "get purse balance"
  1000
  (let ((purse (actormap-poke! am mint 'new-purse 1000)))
    (actormap-peek am purse 'get-balance)))

(let* ((alice-purse (actormap-poke! am mint 'new-purse 1000))
       (bob-purse (actormap-poke! am mint 'new-purse 300))
       (payment-for-bob (actormap-poke! am alice-purse 'sprout)))
  ;; Transfer money from Alice's purse to payment-for-bob.
  (actormap-poke! am payment-for-bob 'deposit 250 alice-purse)
  (test-equal "Alice's balance is decreased"
    750
    (actormap-peek am alice-purse 'get-balance))
  (test-equal "the payment purse has the proper amount in it"
    250
    (actormap-peek am payment-for-bob 'get-balance))
  ;; Give Bob the money.
  (actormap-poke! am bob-purse 'deposit 250 payment-for-bob)
  (test-equal "the payment purse is now empty"
    0
    (actormap-peek am payment-for-bob 'get-balance))
  (test-equal "Bob's balance is increased"
    550
    (actormap-peek am bob-purse 'get-balance)))

;; The 'withdraw' procedure should work just as easily.
(let* ((joe-purse (actormap-poke! am mint 'new-purse 400))
       (payment-for-jane
        (actormap-run! am (lambda () (withdraw 150 joe-purse)))))
  (test-equal "payment for Jane has expected balance"
    150
    (actormap-peek am payment-for-jane 'get-balance))
  (test-equal "Joe's balance is decreased"
    250
    (actormap-peek am joe-purse 'get-balance)))

;; Mallet shouldn't be able make up a new mint and modify the purses
;; from our original mint.
(let* ((mallet-mint (actormap-spawn! am ^mint))
       (mallet-purse (actormap-poke! am mallet-mint 'new-purse 1000))
       (mallet-fraud-purse (actormap-poke! am mint 'new-purse 0)))
  ;; Mallet tries to deposit money from their mint into ours.
  (test-error "Mallet cannot deposit money from their mint into ours"
              (actormap-poke! am mallet-fraud-purse 'deposit 1000 mallet-purse))
  (test-equal "the fraudulent purse balance remains zero"
    0
    (actormap-peek am mallet-fraud-purse 'get-balance)))

;; Deposits and withdrawals shouldn't allow transferring more money
;; than a purse contains.
(let ((zed-purse (actormap-poke! am mint 'new-purse 100))
      (willow-purse (actormap-poke! am mint 'new-purse 0)))
  (test-error "cannot withdraw more than what a purse contains"
              (actormap-run! am (lambda () (withdraw 9000 zed-purse))))
  (test-equal "purse balance remains unchanged after failed withdrawal"
    100
    (actormap-peek am zed-purse 'get-balance))
  (test-error "cannot deposit more than what a purse contains"
              (actormap-poke! am willow-purse 'deposit 9000 zed-purse))
  (test-equal "purse balance remains unchanged after failed deposit"
    0
    (actormap-peek am willow-purse 'get-balance)))

(test-end "test-simple-mint")
