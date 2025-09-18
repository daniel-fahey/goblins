;;; Copyright 2025 Jessica Tallon
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

(define-module (tests actor-lib test-ring-buffer)
  #:use-module (goblins)
  #:use-module (goblins actor-lib ring-buffer)
  #:use-module (tests utils)
  #:use-module (srfi srfi-64))

(test-begin "test-ring-buffer")

(define am (make-actormap))

;; Can spawn a ring buffer
(define rb (actormap-spawn! am ^ring-buffer 5))

(test-assert "Check can spawn a ring buffer"
  (live-refr? rb))

(test-equal "Check empty ring buffer has correct capacity"
  5
  (actormap-peek am rb 'capacity))

(test-equal "Check size of empty ring buffer is zero"
  0
  (actormap-peek am rb 'length))

(test-assert "Ring buffer reports as empty when it's empty"
  (actormap-peek am rb 'empty?))

(test-assert "Ring buffer does not report as full when empty"
  (not (actormap-peek am rb 'full?)))

;; Add some stuff
(actormap-poke! am rb 'put 'one)
(actormap-poke! am rb 'put 'two)
(actormap-poke! am rb 'put 'three)

(test-assert "Ring buffer does not report as empty when partially full"
  (not (actormap-peek am rb 'empty?)))
(test-assert "Ring buffer does not report as full when only partially full"
  (not (actormap-peek am rb 'full?)))

(test-equal "Check head method reads all items in the buffer"
  '(one two three)
  (actormap-peek am rb 'head 3))

(test-equal "Check we can specify a larger value than we have in 'head"
  '(one two three)
  (actormap-peek am rb 'head 5))

(test-equal "Check we can specify a larger value than we have in 'tail"
  '(one two three)
  (actormap-peek am rb 'tail 5))

(test-equal "Check size of partially full ring-buffer is correct"
  3
  (actormap-peek am rb 'length))

;; Add three more things, since it's only of length 5, this will drop
;; one item (the oldest item/first written item).
(actormap-poke! am rb 'put 'four)
(actormap-poke! am rb 'put 'five)
(actormap-poke! am rb 'put 'six)

(test-equal "Check when full ring buffer is written to it loops"
  '(two three four five six)
  (actormap-peek am rb 'head 5))

(test-assert "Ring buffer reports as full when it's full"
  (actormap-peek am rb 'full?))

(test-assert "Ring buffer does not report as empty when full"
  (not (actormap-peek am rb 'empty?)))

(test-equal "Check size of length ring buffer is correct"
  5
  (actormap-peek am rb 'length))

(test-equal "Tail reads correct values"
  '(four five six)
   (actormap-peek am rb 'tail 3))

;; Check we can resize to a bigger size
(actormap-poke! am rb 'resize 7)

(test-equal "Check capacity after resize reads correct"
  7
  (actormap-peek am rb 'capacity))
(test-equal "Check length after resize is correct"
  5
  (actormap-peek am rb 'length))
(test-equal "Check contents are correct after resize"
  '(two three four five six)
  (actormap-peek am rb 'head 5))

(test-equal "Check taking from the ring buffer returns correct value"
  'two
  (actormap-poke! am rb 'take))

(test-equal "Check length after take is correct"
  4
  (actormap-poke! am rb 'length))

(test-equal "Check item in ring buffer after taken"
  '(three four five six)
  (actormap-peek am rb 'head 4))

;; Try resizing small (smaller than current contents)
(actormap-poke! am rb 'resize 3)
(test-equal "Check contents are correct after resize to smaller size"
  '(four five six)
  (actormap-peek am rb 'head 3))
(test-equal "Check length is correct after resize to smaller size"
  3
  (actormap-peek am rb 'length))
(test-equal "Check capacity is correct after resize to smaller size"
  3
  (actormap-peek am rb 'capacity))

;; Persistence
(define-values (am* rb*)
  (persist-and-restore am ring-buffer-env rb))

(test-equal "Check restored ring-buffer has correct elements in"
  '(four five six)
  (actormap-peek am* rb* 'head 3))
(test-equal "Check restored ring-buffer reports correct length"
  3
  (actormap-peek am* rb* 'length))
(test-equal "Check restored ring-buffer reports correct capacity"
  3
  (actormap-peek am* rb* 'capacity))

;; Check clearing ring buffer
(actormap-poke! am rb 'clear)
(test-equal "Check cleared ring buffer has length of zero"
  0
  (actormap-peek am rb 'length))

(test-end "test-ring-buffer")
