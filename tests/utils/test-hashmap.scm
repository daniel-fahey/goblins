;;; Copyright 2025 David Thompson <dave@spritely.institute>
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

(define-module (tests utils hashmaps)
  #:use-module (goblins utils hashmap)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(test-begin "test-hashmap")

;; Basic tests:
(test-equal "lookup of existing key"
  42
  (hashmap-ref (hashmap ('foo 42) ('bar 17)) 'foo))
(test-equal "lookup of missing key"
  #f
  (hashmap-ref (hashmap ('foo 42) ('bar 17)) 'baz))
(test-equal "lookup of remaining key after removal"
  42
  (hashmap-ref (hashmap-remove (hashmap ('foo 42) ('bar 17)) 'bar) 'foo))
(test-equal "lookup of removed key after removal"
  #f
  (hashmap-ref (hashmap-remove (hashmap ('foo 42) ('bar 17)) 'bar) 'bar))
(let ((h (hashmap ('foo 42))))
  (test-eq "insertion of existing key/value is a no-op"
    h
    (hashmap-set h 'foo 42)))
(let ((h (hashmap ('foo 42))))
  (test-eq "removal of missing key is a no-op"
    h
    (hashmap-remove h 'bar)))
(test-equal "fold"
  (iota 1000)
  (sort (hashmap-fold (lambda (k v memo)
                        (cons k memo))
                      '()
                      (fold (lambda (i h)
                              (hashmap-set h i i))
                            (make-hashmap)
                            (iota 1000)))
        <))
(test-equal "for-each"
  499500
  (let ((result 0))
    (hashmap-for-each (lambda (k v)
                        (set! result (+ result v)))
                      (fold (lambda (i h)
                              (hashmap-set h i i))
                            (make-hashmap)
                            (iota 1000)))
    result))

;; This silly hash function helps test some edge cases.  'foo' and
;; 'baz' have a hash collision and 'bar' has mostly the same hash
;; except for some of the least significant bits, ensuring some
;; worst-case code paths are hit.
(define (contrived-hash x n)
  (case x
    ((foo baz) #xf)
    ((bar) #x7)
    (else (error "invalid key" x))))
(define (make-contrived-hashmap)
  (make-hashmap contrived-hash))
(test-equal "hash collisions"
  '(42 17)
  (let* ((h (make-contrived-hashmap))
         (h (hashmap-set h 'foo 42))
         (h (hashmap-set h 'baz 17)))
    (list (hashmap-ref h 'foo) (hashmap-ref h 'baz))))
(test-equal "trie compression upon removal"
  '((foo . 42))
  (hashmap-fold alist-cons '()
                (let* ((h (make-contrived-hashmap))
                       (h (hashmap-set h 'foo 42))
                       (h (hashmap-set h 'bar 17)))
                  (hashmap-remove h 'bar))))

(test-end "test-hashmap")
