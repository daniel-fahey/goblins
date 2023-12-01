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
  #:use-module (srfi srfi-64))

(test-begin "test-queue")

(define am (make-actormap))

(define q (actormap-spawn! am ^queue))
(actormap-poke! am q 'enqueue 'a)
(test-equal (actormap-peek am q 'length) 1)
(test-equal (actormap-peek am q 'empty?) #f)
(actormap-poke! am q 'enqueue 'b)
(actormap-poke! am q 'enqueue 'c)
(test-equal (actormap-peek am q 'length) 3)
(test-equal (actormap-peek am q 'empty?) #f)
(test-equal (actormap-poke! am q 'dequeue) 'a)
(test-equal (actormap-peek am q 'length) 2)
(test-equal (actormap-peek am q 'empty?) #f)
(test-equal (actormap-poke! am q 'dequeue) 'b)
(test-equal (actormap-peek am q 'length) 1)
(test-equal (actormap-peek am q 'empty?) #f)
(actormap-poke! am q 'enqueue 'd)
(actormap-poke! am q 'enqueue 'e)
(test-equal (actormap-peek am q 'length) 3)
(test-equal (actormap-peek am q 'empty?) #f)
(test-equal (actormap-poke! am q 'dequeue) 'c)
(test-equal (actormap-poke! am q 'dequeue) 'd)
(test-equal (actormap-poke! am q 'dequeue) 'e)
(test-equal (actormap-peek am q 'length) 0)
(test-equal (actormap-peek am q 'empty?) #t)

(test-end "test-queue")
