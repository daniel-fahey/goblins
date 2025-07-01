(define-module (tests persistence-store test-memory)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins persistence-store memory)
  #:use-module (goblins core-types)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(test-begin "test-memory-store")

;;; These tests just test that incremental aurie's basic tooling
;;; around revisions work.
;;;
;;; They don't really use the types that are expected for some of the
;;; values, which normally would have portraits, but in this case are
;;; just stubbing in symbols representing a particular generation of
;;; actor.

(define store
  (make-memory-store))

;;; First full snapshot
((persistence-store-save-proc store) 'save-graph
 'some-aurie-id 'whatever
 (let ((ht (make-hash-table)))
   (for-each
    (match-lambda
      ((k v)
       (hashv-set! ht k v)))
    '((0 old0-0)
      (1 old1-0)
      (2 old2-0)
      (3 old3-0)
      (5 old5-0)))
   ht)
 '(0 2))

(test-equal '((0 (delta? #f) (num-portraits 5) (roots (0 2))))
  ((persistence-store-read-proc store) 'get-generations))

(test-equal 'old1-0
  ((persistence-store-read-proc store) 'object-portrait 1))

(test-equal 'old2-0
  ((persistence-store-read-proc store) 'object-portrait 2))

((persistence-store-save-proc store) 'save-delta
 (let ((ht (make-hash-table)))
   (for-each
    (match-lambda
      ((k v)
       (hashv-set! ht k v)))
    '((1 old1-1)
      (5 old5-1)
      (6 old6-1)))
   ht))

(test-equal 'old1-1   ; this changed
  ((persistence-store-read-proc store) 'object-portrait 1))

(test-equal 'old2-0   ; this didn't
  ((persistence-store-read-proc store) 'object-portrait 2))

(test-equal 'old6-1   ; this is new
  ((persistence-store-read-proc store) 'object-portrait 6))

(test-equal '((0 (delta? #f) (num-portraits 5) (roots (0 2)))
              (1 (delta? #t) (num-portraits 3)))
  ((persistence-store-read-proc store) 'get-generations))

;; Now we do a new full persistence.
((persistence-store-save-proc store) 'save-graph
 'some-aurie-id 'whatever
 (let ((ht (make-hash-table)))
   (for-each
    (match-lambda
      ((k v)
       (hashv-set! ht k v)))
    '((0 0-0)
      (1 1-0)
      (2 2-0)))
   ht)
 '(0 1))

(test-equal '1-0
  ((persistence-store-read-proc store) 'object-portrait 1))

(test-equal '2-0
  ((persistence-store-read-proc store) 'object-portrait 2))

(test-equal '((0 (delta? #f) (num-portraits 5) (roots (0 2)))
              (1 (delta? #t) (num-portraits 3))
              (2 (delta? #f) (num-portraits 3) (roots (0 1))))
  ((persistence-store-read-proc store) 'get-generations))

((persistence-store-save-proc store) 'save-delta
 (let ((ht (make-hash-table)))
   (for-each
    (match-lambda
      ((k v)
       (hashv-set! ht k v)))
    '((1 1-1)
      (3 3-1)
      (4 4-1)
      (5 5-1)))
   ht))

((persistence-store-save-proc store) 'save-delta
 (let ((ht (make-hash-table)))
   (for-each
    (match-lambda
      ((k v)
       (hashv-set! ht k v)))
    '((0 0-2)
      (4 4-2)
      (6 6-2)))
   ht))

(test-equal '((0 (delta? #f) (num-portraits 5) (roots (0 2)))
              (1 (delta? #t) (num-portraits 3))
              (2 (delta? #f) (num-portraits 3) (roots (0 1)))
              (3 (delta? #t) (num-portraits 4))
              (4 (delta? #t) (num-portraits 3)))
  ((persistence-store-read-proc store) 'get-generations))

(test-equal '2-0
  ((persistence-store-read-proc store) 'object-portrait 2))

(test-equal '3-1
  ((persistence-store-read-proc store) 'object-portrait 3))

(test-equal '4-2
  ((persistence-store-read-proc store) 'object-portrait 4))

(test-equal '6-2
  ((persistence-store-read-proc store) 'object-portrait 6))

;; And now some tests to make sure we can check old history.

(test-equal 'old0-0
  ((persistence-store-read-proc store)
   'object-portrait 0 #:churn-id 0))

(test-equal 'old1-0
  ((persistence-store-read-proc store)
   'object-portrait 1 #:churn-id 0))

(test-equal 'old6-1
  ((persistence-store-read-proc store)
   'object-portrait 6 #:churn-id 1))

(test-equal 'old1-1
  ((persistence-store-read-proc store)
   'object-portrait 1 #:churn-id 1))

(test-equal '3-1
  ((persistence-store-read-proc store)
   'object-portrait 3 #:churn-id 3))

(test-equal '4-1
  ((persistence-store-read-proc store)
   'object-portrait 4 #:churn-id 3))

(test-equal '4-2
  ((persistence-store-read-proc store)
   'object-portrait 4 #:churn-id 4))

(test-end "test-memory-store")
