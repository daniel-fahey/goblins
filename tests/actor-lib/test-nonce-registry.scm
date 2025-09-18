;;; Copyright 2023 Juliana Sims
;;; Copyright 2024 Jessica Tallon
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

(define-module (tests actor-lib test-swappable)
  #:use-module (goblins core)
  #:use-module (goblins define-actor)
  #:use-module (goblins actor-lib nonce-registry)
  #:use-module (tests utils)
  #:use-module (srfi srfi-64))

(test-begin "test-nonce-registry")

(define am (make-actormap))
(define-actor (^person _bcom name)
  (lambda ()
    name))

(define alice
  (actormap-spawn! am ^person 'i-am-alice))
(define bob
  (actormap-spawn! am ^person 'i-am-bob))

(define-values (registry locator)
  (actormap-run!
   am
   (lambda ()
     (spawn-nonce-registry-and-locator))))

(define alice-swiss-num
  (actormap-poke!
   am
   registry 'register alice))

(define bob-swiss-num
  (actormap-poke!
   am
   registry 'register bob))

(test-assert "swissnums for different objects are not the same"
  (not (equal? alice-swiss-num
               bob-swiss-num)))

(test-equal "the same object registered twice will yield the same swissnum"
  alice-swiss-num
  (actormap-peek am registry 'register alice))

(test-eq "alice swiss num retrieves alice"
  alice
  (actormap-peek
   am
   registry 'fetch alice-swiss-num))

(test-eq "bob swiss num retrieves bob"
  bob
  (actormap-peek
   am
   registry 'fetch bob-swiss-num))

(test-eq "locator fetch and registry fetch retrieve same object"
  (actormap-peek
   am
   registry 'fetch alice-swiss-num)
  (actormap-peek
   am
   locator 'fetch alice-swiss-num))

(test-equal "retrieved objects can be invoked"
  'i-am-bob
  (actormap-peek
   am
   (actormap-peek
    am
    locator 'fetch bob-swiss-num)))

;; Persistence
(define env
  (make-persistence-env
   `((((tests actor-lib test-nonce-registry) ^person) ,^person))
   #:extends nonce-registry-env))
(define-values (am* registry* alice* bob*)
  (persist-and-restore am env registry alice bob))
(test-eq "Alice can be looked up in registry after rehydration"
  alice*
  (actormap-peek am* registry* 'fetch alice-swiss-num))

(test-eq "Bob can be looked up in registry after rehydration"
  bob*
  (actormap-peek am* registry* 'fetch bob-swiss-num))

(test-end "test-nonce-registry")
