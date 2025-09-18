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

(define-module (tests actor-lib test-common)
  #:use-module (goblins core)
  #:use-module (goblins utils hashmap)
  #:use-module (goblins utils ghash)
  #:use-module (goblins actor-lib common)
  #:use-module (tests utils)
  #:use-module (srfi srfi-64))

(test-begin "test-common")

(define am (make-actormap))

(define s (actormap-spawn! am ^seteq 'a 'b 'c))
(test-equal #t (actormap-peek am s 'member? 'c))
(test-equal #f (actormap-peek am s 'member? 'd))
(actormap-poke! am s 'add 'd)
(test-equal #t (actormap-peek am s 'member? 'd))
(actormap-poke! am s 'remove 'c)
(test-equal #f (actormap-peek am s 'member? 'c))
(test-equal "Getting the list of a ^seteq instance"
  (list 'd 'b 'a)
  (actormap-peek am s 'as-list))
(define ghash (actormap-spawn! am ^ghash))
(test-equal "Check key that doesn't exist provides #f when lacking a default"
  #f
  (actormap-peek am ghash 'ref 'foobar))
(test-equal "Check key that doesn't exist provides default when given one"
  'my-default
  (actormap-peek am ghash 'ref 'foobar 'my-default))
(actormap-poke! am ghash 'set 'my-key 'my-value)
(test-equal "Check can get a value from the ghash when one exists"
  'my-value
  (actormap-peek am ghash 'ref 'my-key))
(test-assert "has-key? method returns true when a key exists"
  (actormap-peek am ghash 'has-key? 'my-key))
(test-assert "hash-key? method returns false when a key doesn't exist"
  (not (actormap-peek am ghash 'has-key? 'not-my-key)))
(actormap-poke! am ghash 'set 'foobar 'baz)
(test-assert "Check data method returns a hash with all the values in"
  (let ((data (actormap-peek am ghash 'data)))
    (and (hashmap? data)
         (eq? (hashmap-ref data 'my-key) 'my-value)
         (eq? (hashmap-ref data 'foobar) 'baz)
         (eq? (ghash-length data) 2))))

(actormap-poke! am ghash 'remove 'my-key)
(test-assert
    "Check removing a key means it's no longer in the ghash (relies on has-key? method)"
  (not (actormap-peek am ghash 'has-key? 'my-key)))

;; Vectors
(define vec (actormap-spawn! am ^vector 5 'hello))
(test-equal "Check newly spawned vector has correct length"
  5
  (actormap-peek am vec 'length))

(test-equal "Check newly spawned vector has fill value specified"
  '(hello hello hello hello hello)
   (actormap-peek am vec 'as-list))

(actormap-poke! am vec 'set 2 'goodbye)
(test-equal "Check both set and ref by refing an index we just set"
  'goodbye
  (actormap-peek am vec 'ref 2))

(actormap-poke! am vec 'fill 'salutations)
(test-equal "Check fill method on vectors"
  '(salutations salutations salutations salutations salutations)
   (actormap-peek am vec 'as-list))

(actormap-poke! am vec 'resize 10)
(test-equal "Check we can resize the vector"
  10
  (actormap-peek am vec 'length))

;; Persistence
(define s1 (actormap-spawn! am ^seteq 'a 'b 'c))
(actormap-poke! am s1 'add 'd)
(define gh1 (actormap-spawn! am ^ghash))
(actormap-poke! am gh1 'set 'granny-smith 'green)
(actormap-poke! am gh1 'set 'jazz 'pink)
(define v1 (actormap-spawn! am ^vector 5))
(actormap-poke! am v1 'set 0 'granny-smith)
(actormap-poke! am v1 'set 1 'jazz)
(actormap-poke! am v1 'set 2 'gala)
(define-values (am* s1* gh1* v1*)
  (persist-and-restore am common-env s1 gh1 v1))
(test-equal "Set still has same items in after rehydration"
  (actormap-peek am s1 'as-list)
  (actormap-peek am* s1* 'as-list))
(test-equal "Ghash lookup still works after rehydration"
  'green
  (actormap-peek am* gh1* 'ref 'granny-smith))
(test-equal "Vector still has same elements after rehydration"
  (actormap-peek am v1 'as-list)
  (actormap-peek am* v1* 'as-list))

(test-end "test-common")

