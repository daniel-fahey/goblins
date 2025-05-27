;;; Copyright 2021-2024 Christine Lemmer-Webber
;;; Copyright 2024 Jessica Tallon
;;; Copyright 2025 Juliana Sims
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

;; An immutable hashtable with specific set/ref conventions.  Refrs
;; are hashed by eq?, everything else is hashed by equal?.


(define-module (goblins utils ghash)
  ;; NOTE: Do not depend on core because it depends on us.
  #:use-module (goblins core-types)
  #:use-module (goblins utils hashmap)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)        ; records
  #:use-module (srfi srfi-9 gnu)    ; record extensions
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:export (make-ghash
            ghash?
            ghash

            ghash-set
            ghash-ref
            ghash-remove
            ghash-length
            ghash-has-key?

            ghash-fold
            ghash-for-each

            hash-table->ghash

            make-gset
            gset?
            gset-add
            gset-remove
            gset-length
            gset->list
            gset-member?

            gset-fold
            gset-for-each))

(define (hashmap-length hashmap)
  (hashmap-fold (lambda (_k _v count) (1+ count))
                0 hashmap))

(define (ghash? obj)
  (issue-deprecation-warning
   "`ghash?' is deprecated.  Use `hashmap?' instead.")
  (hashmap? obj))

(define (hashg key size)
  (if (live-refr? key)
      (hashq key size)
      (hash key size)))

(define (equalg? a b)
  (if (or (live-refr? a)
          (live-refr? b))
      (eq? a b)
      (equal? a b)))

(define (make-ghash)
  (make-hashmap hashg equalg?))

(define-syntax ghash
  (syntax-rules ()
    ((_) (make-ghash))
    ((_ (key val) . rest)
     (ghash-set (ghash . rest)
                key val))))

(define (ghash-set ghash key val)
  (issue-deprecation-warning
   "`ghash-set' is deprecated.  Use `hashmap-set' instead.")
  (hashmap-set ghash key val))

(define* (ghash-ref ghash key #:optional [dflt #f])
  (issue-deprecation-warning
   "`ghash-ref' is deprecated.  Use `hashmap-ref' instead.")
  (hashmap-ref ghash key dflt))

(define ghash-has-key?
  (let ((none (cons 'no 'value)))
    (lambda (ghash key)
      (issue-deprecation-warning
       "`ghash-has-key?' is deprecated.  \
Use `hashmap-ref' with a sentinel default value instead.")
      (not (eq? (hashmap-ref ghash key none)
                none)))))

(define (ghash-remove ghash key)
  (issue-deprecation-warning
   "`ghash-remove' is deprecated.  Use `hashmap-remove' instead.")
  (hashmap-remove ghash key))

(define (ghash-length ghash)
  (issue-deprecation-warning
   "`ghash-length' is deprecated.  Use `hashmap-fold' and a counter instead.")
  (hashmap-length ghash))

(define (ghash-fold proc init ghash)
  (issue-deprecation-warning
   "`ghash-fold' is deprecated.  Use `hashmap-fold' instead.")
  (hashmap-fold proc init ghash))

(define (ghash-for-each proc ghash)
  (issue-deprecation-warning
   "`ghash-for-each' is deprecated.  Use `hashmap-for-each' instead.")
  (hashmap-for-each proc ghash))

(define (hash-table->ghash table)
  (issue-deprecation-warning
   "`hash-table->ghash' is deprecated.  \
Use `hash-fold' initialized to `make-ghash' and populated using `hashmap-set' \
instead.")
  (hash-fold
   (lambda (key val hm)
     (hashmap-set hm key val))
   (make-ghash) table))

;;; Sets
(define-record-type <gset>
  (%make-gset hashmap)
  gset?
  (hashmap gset-hashmap))

(define (print-gset gset port)
  (format port "#<gset ~a>" (gset->list gset)))

(set-record-type-printer! <gset> print-gset)

(define (make-gset . items)
  (%make-gset
   (fold
    (lambda (item hm)
      (hashmap-set hm item #t))
    (make-ghash) items)))

(define (gset-add set item)
  (let* ((hm (gset-hashmap set))
         (maybe-new-hm (hashmap-set hm item #t)))
    (if (eq? maybe-new-hm hm)
        set
        (%make-gset maybe-new-hm))))

(define (gset-remove set item)
  (let* ((hm (gset-hashmap set))
         (maybe-new-hm (hashmap-remove hm item)))
    (if (eq? maybe-new-hm hm)
        set
        (%make-gset maybe-new-hm))))

(define (gset-fold proc init set)
  (hashmap-fold
   (lambda (key _ prev)
     (proc key prev))
   init (gset-hashmap set)))

(define (gset-length set)
  (hashmap-length (gset-hashmap set)))

(define (gset->list set)
  (hashmap-fold
   (lambda (key _ prev)
     (cons key prev))
   '() (gset-hashmap set)))

(define (gset-member? set key)
  (hashmap-ref (gset-hashmap set) key))

(define (gset-for-each proc set)
  (hashmap-for-each
   (lambda (k _)
     (proc k))
   (gset-hashmap set)))
