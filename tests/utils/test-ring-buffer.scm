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

(define-module (tests utils ring-buffer)
  #:use-module (goblins utils ring-buffer)
  #:use-module (srfi srfi-64))

(test-begin "test-ring-buffer")

(test-assert "Empty ring buffer is empty"
  (ring-buffer-empty? (make-ring-buffer 1)))

(test-assert "Non-empty ring buffer is not empty"
  (let ((ring (make-ring-buffer 1)))
    (ring-buffer-put! ring 'foo)
    (not (ring-buffer-empty? ring))))

(test-assert "Full ring buffer is full"
  (let ((ring (make-ring-buffer 1)))
    (ring-buffer-put! ring 'foo)
    (ring-buffer-full? ring)))

(test-assert "Non-full ring buffer is not full"
  (let ((ring (make-ring-buffer 2)))
    (ring-buffer-put! ring 'foo)
    (not (ring-buffer-full? ring))))

(test-eqv "Putting an item increments length"
  1
  (let ((ring (make-ring-buffer 2)))
    (ring-buffer-put! ring 'foo)
    (ring-buffer-length ring)))

(test-eq "Putting an item overwrites oldest item when over capacity"
  'bar
  (let ((ring (make-ring-buffer 2)))
    (ring-buffer-put! ring 'foo)
    (ring-buffer-put! ring 'bar)
    (ring-buffer-put! ring 'baz)
    (ring-buffer-ref ring 0)))

(test-assert "Getting returns oldest item and removes it from buffer"
  (let ((ring (make-ring-buffer 2)))
    (ring-buffer-put! ring 'foo)
    (ring-buffer-put! ring 'bar)
    (and (eq? (ring-buffer-get! ring) 'foo)
         (eq? (ring-buffer-ref ring 0) 'bar))))

(test-eqv "Getting decrements length"
  0
  (let ((ring (make-ring-buffer 2)))
    (ring-buffer-put! ring 'foo)
    (ring-buffer-get! ring)
    (ring-buffer-length ring)))

(test-error "Getting throws an error when empty"
            #t
            (ring-buffer-get! (make-ring-buffer 1)))

(test-eq "Refing returns item at index"
  'foo
  (let ((ring (make-ring-buffer 1)))
    (ring-buffer-put! ring 'foo)
    (ring-buffer-ref ring 0)))

(test-error "Refing throws an error when index is out of bounds"
            #t
            (ring-buffer-ref (make-ring-buffer 1) 0))

(test-eqv "Resizing increases capacity"
  2
  (let ((ring (make-ring-buffer 1)))
    (ring-buffer-resize! ring 2)
    (ring-buffer-capacity ring)))

(test-equal "Resizing retains existing items"
  '(foo bar)
  (let ((ring (make-ring-buffer 2)))
    (ring-buffer-put! ring 'foo)
    (ring-buffer-put! ring 'bar)
    (ring-buffer-resize! ring 3)
    (list (ring-buffer-ref ring 0)
          (ring-buffer-ref ring 1))))

(test-equal "Resizing deletes oldest items when capacity is decreased"
  '(baz zort)
  (let ((ring (make-ring-buffer 3)))
    (ring-buffer-put! ring 'foo)
    (ring-buffer-put! ring 'bar)
    (ring-buffer-put! ring 'baz)
    (ring-buffer-resize! ring 2)
    (ring-buffer-put! ring 'zort)
    (list (ring-buffer-ref ring 0)
          (ring-buffer-ref ring 1))))

(test-equal "Resizing sets up the head/tail pointers correctly for future puts"
  '(baz zort narf troz)
  (let ((ring (make-ring-buffer 3)))
    (ring-buffer-put! ring 'foo)
    (ring-buffer-put! ring 'bar)
    (ring-buffer-put! ring 'baz)
    (ring-buffer-put! ring 'zort)
    (ring-buffer-resize! ring 4)
    (ring-buffer-put! ring 'narf)
    (ring-buffer-put! ring 'troz)
    (list (ring-buffer-ref ring 0)
          (ring-buffer-ref ring 1)
          (ring-buffer-ref ring 2)
          (ring-buffer-ref ring 3))))

(test-assert "Clearing empties the buffer"
  (let ((ring (make-ring-buffer 1)))
    (ring-buffer-put! ring 'foo)
    (ring-buffer-clear! ring)
    (ring-buffer-empty? ring)))

(test-end "test-ring-buffer")
