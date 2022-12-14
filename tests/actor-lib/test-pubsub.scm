;;; Copyright 2020-2021 Christine Lemmer-Webber
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

(define-module (tests actor-lib test-pubsub)
  #:use-module (goblins core)
  #:use-module (goblins actor-lib pubsub)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(test-begin "test-pubsub")

(define am (make-actormap))

(define* (^listener bcom #:optional (heard-back '()))
  (lambda msg
    (if (null? msg)
        heard-back
        (bcom (^listener bcom (cons msg heard-back)) heard-back))))

(define listener1 (actormap-spawn! am ^listener))
(define listener2 (actormap-spawn! am ^listener))
(define listener3 (actormap-spawn! am ^listener))

(define pubsub (actormap-spawn! am ^pubsub listener1 listener2))

(actormap-churn-run!
  am
 (lambda ()
   ($ pubsub 'publish 'first 1 2 3)))

(test-equal
    "First subscribed listener got published message"
  (car (actormap-peek am listener1))
  '(first 1 2 3))

(test-equal
    "Second subscribed listener got published message"
  (car (actormap-peek am listener2))
  '(first 1 2 3))

(test-equal
    "Third not-yet-subscribed listener didn't get the message"
  (actormap-peek am listener3)
  '())

(test-assert
    "Check the two subscribers are present in the subscriber list"
  (let ((subscribers (actormap-peek am pubsub 'subscribers)))
    (and (eq? (length subscribers) 2)
         (find (lambda (sub) (eq? sub listener1)) subscribers)
         (find (lambda (sub) (eq? sub listener2)) subscribers))))

;; Test adding a subscriber and sending a message
(actormap-poke! am pubsub 'subscribe listener3)
(test-assert
    "Check the three subscribers are present in the subscriber list"
  (let ((subscribers (actormap-peek am pubsub 'subscribers)))
    (and (eq? (length subscribers) 3)
         (find (lambda (sub) (eq? sub listener1)) subscribers)
         (find (lambda (sub) (eq? sub listener2)) subscribers)
         (find (lambda (sub) (eq? sub listener3)) subscribers))))

(actormap-churn-run!
 am
 (lambda ()
   ($ pubsub 'publish 'second)))

(test-equal
    "Check first subscribed listener got the published message"
  (car (actormap-peek am listener1))
  '(second))

(test-equal
    "Check second subscribed listener got the published message"
  (car (actormap-peek am listener2))
  '(second))

(test-equal
    "Check third subscribed listener got the published message"
  (car (actormap-peek am listener3))
  '(second))

;; Test removing a subscriber and sending a message
(actormap-poke! am pubsub 'unsubscribe listener1)
(test-assert
    "Check the three subscribers are present in the subscriber list"
  (let ((subscribers (actormap-peek am pubsub 'subscribers)))
    (and (eq? (length subscribers) 2)
         (find (lambda (sub) (eq? sub listener2)) subscribers)
         (find (lambda (sub) (eq? sub listener3)) subscribers))))

(actormap-churn-run!
 am
 (lambda ()
   ($ pubsub 'publish 'third)))

(test-equal
    "Check first no longer subscribed listener didn't get the message"
  (car (actormap-peek am listener1))
  '(second))

(test-equal
    "Check second subscribed listener got the message"
  (car (actormap-peek am listener2))
  '(third))

(test-equal
    "Check third subscribed listener got the message"
  (car (actormap-peek am listener3))
  '(third))

(test-end "test-pubsub")
