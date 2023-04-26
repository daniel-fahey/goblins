;;; Copyright 2023 Juliana Sims
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
  #:use-module (goblins actor-lib nonce-registry)
  #:use-module (srfi srfi-64))

(test-begin "test-nonce-registry")

(define am (make-actormap))

(define alice
  (actormap-spawn! am (lambda _ (lambda _ 'i-am-alice))))
(define bob
  (actormap-spawn! am (lambda _ (lambda _ 'i-am-bob))))

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

(test-expect-fail 2)
(test-equal "swiss nums for different objects are not the same"
  alice-swiss-num
  bob-swiss-num)

(test-equal "swiss nums for same object are not the same"
  alice-swiss-num
  (actormap-poke!
   am
   registry 'register alice))

(test-eq "alice swiss num retrieves alice"
  (actormap-peek
   am
   registry 'fetch alice-swiss-num)
  alice)

(test-eq "bob swiss num retrieves bob"
  (actormap-peek
   am
   registry 'fetch bob-swiss-num)
  bob)

(test-eq "locator fetch and registry fetch retrieve same object"
  (actormap-peek
   am
   registry 'fetch alice-swiss-num)
  (actormap-peek
   am
   locator 'fetch alice-swiss-num))

(test-equal "retrieved objects can be invoked"
  (actormap-peek
   am
   (actormap-peek
    am
    locator 'fetch bob-swiss-num))
  'i-am-bob)

(test-end "test-nonce-registry")
