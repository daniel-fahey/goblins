;;; Copyright 2019-2023 Christine Lemmer-Webber
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

(define-module (tests actor-lib test-pushdown)
  #:use-module (goblins)
  #:use-module (goblins define-actor)
  #:use-module (goblins actor-lib pushdown)
  #:use-module (tests utils)
  #:use-module (srfi srfi-64))

(test-begin "test-pushdown")

(define am (make-actormap))
(define-actor (^actor _bcom prev one two)
  (lambda (three)
    (list one prev two three)))

(define-values (pd-stack pd-forwarder)
  (actormap-run! am spawn-pushdown-pair))

(define first-actor
  (actormap-poke! am pd-stack 'spawn-push
		  ^actor 'first 'foo))
(test-equal "Pushdown forwarder sends to first actor if only one on stack"
 `(first #f foo bar)
 (actormap-poke! am pd-forwarder 'bar))

(define second-actor
  (actormap-poke! am pd-stack 'spawn-push
		  ^actor 'second 'foo2))
(test-equal "Pushdown forwarder sends to second actor once pushed onto stack"
 `(second ,first-actor foo2 bar2)
 (actormap-poke! am pd-forwarder 'bar2))

(actormap-poke! am pd-stack 'pop)

(test-equal "Pushdown forwarder sends to first actor again after first actor popped off"
 `(first #f foo bar3)
 (actormap-poke! am pd-forwarder 'bar3))

(define third-actor
  (actormap-spawn! am ^actor #f 'third 'foo3))

(actormap-poke! am pd-stack 'push third-actor)

(test-equal "Pushdown stack 'push method works too"
  '(third #f foo3 bar4)
  (actormap-poke! am pd-forwarder 'bar4))

;; Persistence
(define-values (pd-stack pd-forwarder)
  (actormap-run! am spawn-pushdown-pair))
(define first-actor
  (actormap-poke! am pd-stack 'spawn-push
		  ^actor 'first 'foo))
(define second-actor
  (actormap-poke! am pd-stack 'spawn-push
		  ^actor 'second 'foo2))
(define env
  (make-persistence-env
   `((((tests actor-lib test-pushdown) ^actor) ,^actor))
   #:extends pushdown-env))

(define-values (am* pd-stack* pd-forwarder* first* second*)
  (persist-and-restore am env pd-stack pd-forwarder first-actor second-actor))

(test-equal "After rehydration pushdown sends to second actor"
  `(second ,first* foo2 bar2)
  (actormap-poke! am* pd-forwarder* 'bar2))

(test-end "test-pushdown")
