;;; Copyright 2023 Christine Lemmer-Webber
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

(define-module (tests actor-lib test-queue)
  #:use-module (goblins core)
  #:use-module (goblins actor-lib queue)
  #:use-module (tests utils)
  #:use-module (srfi srfi-64))

(test-begin "test-queue")

(define am (make-actormap))

(define q (actormap-spawn! am ^queue))
(actormap-poke! am q 'enqueue 'a)
(test-equal 1 (actormap-peek am q 'length))
(test-equal #f (actormap-peek am q 'empty?))
(actormap-poke! am q 'enqueue 'b)
(actormap-poke! am q 'enqueue 'c)
(test-equal 3 (actormap-peek am q 'length))
(test-equal #f (actormap-peek am q 'empty?))
(test-equal 'a (actormap-poke! am q 'dequeue))
(test-equal 2 (actormap-peek am q 'length))
(test-equal #f (actormap-peek am q 'empty?))
(test-equal 'b (actormap-poke! am q 'dequeue))
(test-equal 1 (actormap-peek am q 'length))
(test-equal #f (actormap-peek am q 'empty?))
(actormap-poke! am q 'enqueue 'd)
(actormap-poke! am q 'enqueue 'e)
(test-equal 3 (actormap-peek am q 'length))
(test-equal #f (actormap-peek am q 'empty?))
(test-equal 'c (actormap-poke! am q 'dequeue))
(test-equal 'd (actormap-poke! am q 'dequeue))
(test-equal 'e (actormap-poke! am q 'dequeue))
(test-equal 0 (actormap-peek am q 'length))
(test-assert (actormap-peek am q 'empty?))

;; Persistence
(define q1 (actormap-spawn! am ^queue))
(actormap-poke! am q1 'enqueue 'a)
(actormap-poke! am q1 'enqueue 'b)
(define-values (am* q1*)
  (persist-and-restore am queue-env q1))
(test-equal 2 (actormap-peek am* q1* 'length))
(test-equal #f (actormap-peek am* q1* 'empty?))
(test-equal 'a (actormap-poke! am* q1* 'dequeue))
(test-equal 1 (actormap-peek am* q1* 'length))
(test-equal 'b (actormap-poke! am* q1* 'dequeue))
(test-equal #t (actormap-peek am* q1* 'empty?))

(test-end "test-queue")
