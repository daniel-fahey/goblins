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

(define-module (tests persistence-store test-bloblin)
  #:use-module (goblins)
  #:use-module (goblins core-types)
  #:use-module (goblins persistence-store bloblin)
  #:use-module (goblins utils crypto)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(test-begin "test-bloblin")

(define tempdir (mkdtemp "/tmp/goblins-test-XXXXXX"))
(define store (make-bloblin-store tempdir))
(define vat-aurie-id (strong-random-bytes 32))

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

;; An empty store should return #f for everything when asked for
;; the graph and roots. This test **must** come before we write to it.
(test-equal "Reading graph and slots from an empty store gives #f"
 '(#f #f #f #f)
 (call-with-values
  (lambda () ((persistence-store-read-proc store) 'graph-and-slots)) list))

((persistence-store-save-proc store) 'save-graph
 vat-aurie-id 'version-0
 (let ((ht (make-hash-table)))
   (for-each
    (match-lambda
      ((k v)
       (hashv-set! ht k v)))
    `((0 ,(make-object-portrait 'obj0))
      (1 ,(make-object-portrait 'obj1))
      (2 ,(make-object-portrait 'obj2))
      (3 ,(make-object-portrait 'obj3))
      (4 ,(make-object-portrait 'obj4))))
   ht)
 '(0 2))

(test-equal "Can get first object portrait with 'object-portrait"
  (make-object-portrait 'obj1)
  ((persistence-store-read-proc store) 'object-portrait 1))

(test-equal "Can get second object portrait with 'object-portrait"
   (make-object-portrait 'obj2)
  ((persistence-store-read-proc store) 'object-portrait 2))

((persistence-store-save-proc store) 'save-delta
 (let ((ht (make-hash-table)))
   (for-each
    (match-lambda
      ((k v)
       (hashv-set! ht k v)))
    `((1 ,(make-object-portrait 'obj1 #:version 1))
      (4 ,(make-object-portrait 'obj4 #:version 1))
      ;; New
      (6 ,(make-object-portrait 'obj6))))
   ht))

;; object 1 changed.
(test-equal "Get just changed object portrait after 'save-delta"
  (make-object-portrait 'obj1 #:version 1)
  ((persistence-store-read-proc store) 'object-portrait 1))

(test-equal "Get portrait which did not change after 'save-delta"
  (make-object-portrait 'obj2)
  ((persistence-store-read-proc store) 'object-portrait 2))

;; New object
(test-equal "Get entirely new portrait saved with 'save-delta"
  (make-object-portrait 'obj6)
  ((persistence-store-read-proc store) 'object-portrait 6))

;; Can get whole object graph after delta store
(define-values (read-vat-aurie-id read-roots-version read-portraits read-root-slots)
  ((persistence-store-read-proc store) 'graph-and-slots))

(test-equal "'graph-and-slots provides correct vat-aurie-id"
  vat-aurie-id
  read-vat-aurie-id)

(test-equal "'graph-and-slots provides correct roots version"
  'version-0
  read-roots-version)

(test-assert "'graph-and-slots provides correct portraits"
  (hash-table-equal?
   (let ((ht (make-hash-table)))
     (for-each
      (match-lambda
        ((k v) (hash-set! ht k v)))
      `((0 ,(make-object-portrait 'obj0))
        (1 ,(make-object-portrait 'obj1 #:version 1))
        (2 ,(make-object-portrait 'obj2))
        (3 ,(make-object-portrait 'obj3))
        (4 ,(make-object-portrait 'obj4 #:version 1))
        (6 ,(make-object-portrait 'obj6))))
     ht)
   read-portraits))

;; Hook it up to something real just to make sure.
(define-actor (^greeter _bcom our-name)
  (lambda (their-name)
    (format #f "Hello ~a, my name is ~a" their-name our-name)))
(define-actor (^incrementer bcom #:optional (number 0))
  (lambda ()
    (bcom (^incrementer bcom (1+ number)) (1+ number))))
(define env
  (make-persistence-env
   `((((tests persistence-store bloblin) ^greeter) ,^greeter)
     (((tests persistence-store bloblin) ^incrementer) ,^incrementer))))

(define tempdir (mkdtemp "/tmp/goblins-test-XXXXXX"))
(define store (make-bloblin-store tempdir))
(define-values (vat alice bob incrementer)
  (spawn-persistent-vat
   env
   (lambda ()
     (values (spawn ^greeter "Alice")
             (spawn ^greeter "Bob")
             (spawn ^incrementer)))
   store))

;; Call things which shouldn't become and shouldn't change...
(with-vat vat
  ($ alice "Carol")
  ($ bob "Carol"))
;; Also call things which do change and should create deltas
(with-vat vat
  ($ incrementer)
  ($ incrementer))
(vat-halt! vat)

;; Respawn and check all is well
(define-values (vat* alice* bob* incrementer*)
  (spawn-persistent-vat
   env
   (lambda () (error "should read from store"))
   store))

(test-equal "Check can use bloblin store with vats"
  3
  (with-vat vat* ($ incrementer*)))

(test-end "test-bloblin")