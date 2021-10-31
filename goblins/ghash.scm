;;; Copyright 2021 Christine Lemmer-Webber
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
;;
;; TODO: Really presently built on top of vhashes.  Might be built on
;; top of something else, like fashes, in the future.  Especially since
;; vhashes are not thread safe...


(define-module (goblins ghash)
  #:use-module (goblins core)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:export (make-ghash
            ghash?

            ghash-set
            ghash-ref
            ghash-remove
            ghash-null
            ghash-length

            ghash-fold
            ghash-fold-right
            ghash-for-each

            hash-table->ghash))


(define-record-type <ghash>
  (_make-ghash vhash)
  ghash?
  (vhash ghash-vhash))

(define ghash-null (_make-ghash vlist-null))

(define (make-ghash . key-vals)
  (_make-ghash
   (let lp ((key-vals key-vals)
            (vh vlist-null))
     (match key-vals
       [() vh]
       [(key val rest ...)
        (lp rest
            (_vh-set vh key val))]))))

(define (_vh-set vh key val)
  (define conser
    (if (live-refr? key)
        vhash-consq
        vhash-cons))
  (conser key val vh))

(define (ghash-set ghash key val)
  (define vh (ghash-vhash ghash))
  (_make-ghash (_vh-set vh key val)))

(define* (ghash-ref ghash key #:optional [dflt #f])
  (define vh (ghash-vhash ghash))
  (define assoc
    (if (live-refr? key)
        vhash-assq
        vhash-assoc))
  (match (assoc key vh)
    ((_k . val) val)
    (#f dflt)))

(define (ghash-remove ghash key)
  (define vh (ghash-vhash ghash))
  (define del
    (if (live-refr? key)
        vhash-delq
        vhash-delete))
  (_make-ghash (del key vh)))

(define (ghash-length ghash)
  (vlist-length (ghash-vhash ghash)))

(define (ghash-fold proc init ghash)
  (vhash-fold proc init (ghash-vhash ghash)))
(define (ghash-fold-right proc init ghash)
  (vhash-fold-right proc init (ghash-vhash ghash)))

(define (ghash-for-each proc ghash)
  (vhash-fold
   (lambda (k v _p)
     (proc k v))
   #f
   (ghash-vhash ghash)))

(define (hash-table->ghash table)
  (_make-ghash
   (hash-fold
    (lambda (key val vh)
      (_vh-set vh key val))
    vlist-null
    table)))

