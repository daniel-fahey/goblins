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

(define-module (tests utils persistence)
  #:use-module (goblins abstract-types)
  #:use-module (goblins utils ghash)
  #:use-module (goblins utils hashmap)
  #:use-module (goblins utils persistence)
  #:use-module (srfi srfi-64))

(test-begin "test-persistence")

;; The following three procedures are from (tests persistence-store bloblin),
;; maybe we should consider factoring them out into a testing util.
;; Helper function to make the code where it's used easier to read.
(define* (make-object-portrait object-name
                               #:key
                               (debug-name 'obj)
                               (version 0)
                               (portrait '()))
  (list object-name debug-name version portrait))
(define (hash-table-length ht)
  (hash-fold (lambda (k v s) (1+ s)) 0 ht))
(define (hash-table-equal? ht1 ht2)
  (and (hash-table? ht1)
       (hash-table? ht2)
       (= (hash-table-length ht1) (hash-table-length ht2))
       (hash-fold
        (lambda (key value prev)
          (and prev (equal? (hash-ref ht2 key) value)))
        #t
        ht1)))

;; Make a graph which has no orphans, they should be identical when run through
;; the orphan pruning procedure, here's the following graph:
;;      0      2
;;      |     / \
;;      1     3 4
(define portraits-1 (make-hash-table))
;; Root 0:
(hash-set! portraits-1 0
           (make-object-portrait 'obj0 #:portrait (list (make-tagged* 'near 1))))
(hash-set! portraits-1 1 (make-object-portrait 'obj1 #:portrait (list 1 2 3)))
;; Root 2:
(hash-set! portraits-1 2
           (make-object-portrait 'obj0
                                 #:portrait (list 'foo 1 2
                                                  (make-tagged* 'near 3)
                                                  (make-tagged* 'near 4))))
(hash-set! portraits-1 3 (make-object-portrait 'obj1 #:portrait (list 'beep 'boop)))
(hash-set! portraits-1 4 (make-object-portrait 'obj1 #:portrait (list 1 2)))

(test-assert "Check object with no orphans is the same after remove-orphaned-objects"
  (hash-table-equal? portraits-1
                     (remove-orphaned-objects portraits-1 (list 0 2))))

;; Next graph, lets have two orphans
;; Here's the graph
;;     0      2
;;     |     / \
;;     1    4   6
;;          |
;;          7
;;
;; 3 and 5 are orphans
(define portraits-2 (make-hash-table))
(hash-set! portraits-2 0
           (make-object-portrait 'obj0 #:portrait (list (make-tagged* 'near 1))))
(hash-set! portraits-2 1 (make-object-portrait 'obj1 #:portrait (list 1 2 3)))
;; Next root
(hash-set! portraits-2 2
           (make-object-portrait 'obj3 #:portrait (list #t #f
                                                        (make-tagged* 'near 4)
                                                        (make-tagged* 'near 6))))
(hash-set! portraits-2 4
           (make-object-portrait 'obj3 #:portrait (list (make-tagged* 'near 7)
                                                        (make-tagged* 'far 'something 6))))
(hash-set! portraits-2 7 (make-object-portrait 'obj7))
;; Make this introduce a cycle, point it back at 2
(hash-set! portraits-2 6
           (make-object-portrait 'obj6 #:portrait (list (make-tagged* 'near 2))))

;; Add the orphans
(hash-set! portraits-2 3 (make-object-portrait 'obj0 #:portrait (make-tagged* 5)))
(hash-set! portraits-2 5 (make-object-portrait 'obj5 #:portrait (list 'apple 'banana)))

(define processed-portraits-2 (remove-orphaned-objects portraits-2 (list 0 2)))
(test-assert "remove-orphaned-objects orphans from graph"
  (and (= (hash-table-length processed-portraits-2) 6)
       (hash-ref processed-portraits-2 0)
       (hash-ref processed-portraits-2 1)
       (hash-ref processed-portraits-2 2)
       (hash-ref processed-portraits-2 4)
       (hash-ref processed-portraits-2 7)
       (hash-ref processed-portraits-2 6)))

;; Check data with nested objects
;; 0         2              4        6        8
;; | (list)  | (hash-table) | (vec)  | (gset) | (tagged)
;; 1         3              5        7        9

(define portraits-3 (make-hash-table))
;; Within a list.
(hash-set! portraits-3 0
           (make-object-portrait 'obj0 #:portrait (list (list 0 1 (make-tagged* 'near 1)))))
(hash-set! portraits-3 1 (make-object-portrait 'obj1))
;; Within a hash table
(hash-set! portraits-3 2
           (make-object-portrait 'obj0 #:portrait (list (hashmap ('foo (make-tagged* 'near 3))))))
(hash-set! portraits-3 3 (make-object-portrait 'obj1))

;; Vectors
(define vec (make-tagged* 'vec (list #f #f (make-tagged* 'near 5) #f #f)))
(hash-set! portraits-3 4 (make-object-portrait 'obj0 #:portrait (list vec)))
(hash-set! portraits-3 5 (make-object-portrait 'obj1))

;; gset
(define gset (make-gset 'hi (make-tagged* 'near 7) 'bye))
(hash-set! portraits-3 6 (make-object-portrait 'obj0 #:portrait (list gset)))
(hash-set! portraits-3 7 (make-object-portrait 'obj1))

;; Tagged
(define tagged (make-tagged* 'tagged `(some-label ,(make-tagged* 'near 9))))
(hash-set! portraits-3 8 (make-object-portrait 'obj0 #:portrait (list tagged)))
(hash-set! portraits-3 9 (make-object-portrait 'obj1))

(define processed-portraits-3 (remove-orphaned-objects portraits-3 (list 0 2 4 6 8)))
(test-assert "remove-orphaned-objects can handle nested near refrs"
  (hash-table-equal? portraits-3 processed-portraits-3))


(test-end "test-persistence")