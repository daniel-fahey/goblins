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
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins persistence-store bloblin)
  #:use-module (goblins utils crypto)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(test-begin "test-bloblin")

(define (recursive-delete target)
  (define (skip-file? name)
    ;; Catches both .. and .
    (string-suffix? "." name))
  (when (and (file-exists? target) (not (skip-file? target)))
    (match (stat:type (stat target))
      ('regular (delete-file target))
      ('directory
       ;; Find all the stuff in the dir, and delete those.
       (let ((dir (opendir target)))
         (let lp ((entry (readdir dir)))
           (unless (eof-object? entry)
             (recursive-delete (string-append target file-name-separator-string entry))
             (lp (readdir dir))))
         (closedir dir)
         ;; Now it's empty, remove the dir
         (rmdir target)))
      (other-type (error "recursive-delete only works on regular directories and files")))))

(define-syntax-rule (call-with-temp-dir tmpl body-proc)
  (let ((dir (mkdtemp tmpl)))
    (body-proc dir)
    (recursive-delete dir)))

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

;; Glorified `ls *.bloblin`
(define (bloblins-in-dir dirname)
  (define (bloblin-file? filename)
    (string-suffix? ".bloblin" filename))
  (define dir (opendir dirname))
  (define bloblins
    (let lp ((entry (readdir dir)))
      (match entry
        ;; Skip the
        ((? eof-object?) '())
        ((? bloblin-file?) (cons entry (lp (readdir dir))))
        (_ (lp (readdir dir))))))
  (closedir dir)
  ;; I think these should probably be in order? sort them anyway...
  (sort bloblins string<?))

;; Make a folder for bloblin data.
(call-with-temp-dir "bloblin-data-XXXXXX"
  (lambda (tempdir)
    (define store (make-bloblin-store tempdir))
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
       read-portraits))))

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

(call-with-temp-dir "bloblin-data-XXXXXX"
  (lambda (tempdir)
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
      (with-vat vat* ($ incrementer*)))))

;; Check that bloblin creates a new file after deltas-per-file reached.
(call-with-temp-dir "bloblin-data-XXXXXX"
  (lambda (tempdir)
    (define store (make-bloblin-store tempdir #:deltas-per-file 3))
    (define-values (vat my-cell)
      (spawn-persistent-vat
       cell-env
       (lambda ()
         (spawn ^cell))
       store))

    ;; Sanity check we start with one file
    (test-equal "Bloblin begins by writing one bloblin file"
      '("0.bloblin")
      (bloblins-in-dir tempdir))

    ;; Each `with-vat` should be one churn, and deltas are written per churn.
    ;; Check that after 3 of them (deltas-per-file value), we get another file.
    ;; First lets just verify we just have the one churn file
    (with-vat vat ($ my-cell 1))
    (with-vat vat ($ my-cell 2))

    ;; After creating deltas - 1 (check we still only have one file)
    (test-equal "Bloblin writes deltas and doesn't create a new file until reached limit"
      '("0.bloblin")
      (bloblins-in-dir tempdir))

    ;; Finally write the next delta, creating the file
    (with-vat vat ($ my-cell 3))

    (test-equal "Bloblin creates a new file once the deltas-in-file has been reached"
      '("0.bloblin" "1.bloblin")
      (bloblins-in-dir tempdir))

    ;; Remove the first file and check we can still restore
    (vat-halt! vat)

    ;; Delete the initial portrait, this lets us check we can restore from the
    ;; second portrait file.
    (delete-file (string-append tempdir file-name-separator-string "0.bloblin"))

    (define-values (vat* my-cell*)
      (spawn-persistent-vat
       cell-env
       (lambda ()
         (spawn ^cell))
       (make-bloblin-store tempdir #:deltas-per-file 3)))

    (test-equal "Check our cell has the value we wrote to it"
      3
      (with-vat vat* ($ my-cell*)))))

;; Test old file cleanup
(call-with-temp-dir "bloblin-data-XXXXXX"
  (lambda (tempdir)
    (define store (make-bloblin-store tempdir
                                      #:deltas-per-file 2
                                      #:max-bloblin-files 2))
    (define-values (vat my-cell)
      (spawn-persistent-vat
       cell-env
       (lambda ()
         (spawn ^cell))
       store))

    ;; Lets make what should be 3 files, one of which should be deleted.
    ;; each churn will produce one delta, so 6 churns are what we need todo.
    (with-vat vat ($ my-cell 1))
    (with-vat vat ($ my-cell 2))
    ;; 2nd file
    (with-vat vat ($ my-cell 3))
    (with-vat vat ($ my-cell 4))
    ;; Starting the 3rd file (0.bloblin should be deleted)
    (with-vat vat ($ my-cell 5))

    (test-equal "Old bloblin files are cleaned up"
      '("1.bloblin" "2.bloblin")
      (bloblins-in-dir tempdir))))

(test-end "test-bloblin")
