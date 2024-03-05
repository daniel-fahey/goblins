;;; Copyright 2021-2024 Christine Lemmer-Webber
;;; Copyright 2024 Jessica Tallon
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
  ;; NOTE: Do not depend on core because it depends on us.
  #:use-module (goblins core-types)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)        ; records
  #:use-module (srfi srfi-9 gnu)    ; record extensions
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:export (make-ghash
            ghash?

            ghash-set
            ghash-ref
            ghash-remove
            ghash-null
            ghash-length
            ghash-has-key?

            ghash-fold
            ghash-fold-right
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


(define-record-type <ghash>
  (_make-ghash vhash)
  ghash?
  (vhash ghash-vhash))

(define (vhash-length vhash)
  (vhash-fold (lambda (_k _v count) (1+ count))
              0 vhash))

(define (print-ghash vhash port)
  (format port "#<ghash (~a)>"
          (vhash-length (ghash-vhash vhash))))

(set-record-type-printer! <ghash> print-ghash)

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
    (if (or (live-refr? key) (symbol? key))
        vhash-consq
        vhash-cons))
  (conser key val vh))

(define (ghash-set ghash key val)
  (define vh (ghash-vhash ghash))
  (_make-ghash (_vh-set vh key val)))

(define* (ghash-ref ghash key #:optional [dflt #f])
  (define vh (ghash-vhash ghash))
  (define assoc
    (if (or (live-refr? key) (symbol? key))
        vhash-assq
        vhash-assoc))
  (match (assoc key vh)
    ((_k . val) val)
    (#f dflt)))

(define (ghash-has-key? ghash key)
  (define vh (ghash-vhash ghash))
  (define assoc
    (if (or (live-refr? key) (symbol? key))
        vhash-assq
        vhash-assoc))
  (match (assoc key vh)
    ((_k . val) #t)
    (#f #f)))

(define (ghash-remove ghash key)
  (define vh (ghash-vhash ghash))
  (define del
    (if (or (live-refr? key) (symbol? key))
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

;;; Sets
(define-record-type <gset>
  (_make-gset ht)
  gset?
  (ht _set-ht))

(define (print-set set port)
  (define items
    (vhash-fold
     (lambda (k _v prev)
       (cons k prev))
     '()
     (_set-ht set)))
  (format port "#<gset ~a>" items))

(set-record-type-printer! <gset> print-set)

(define (make-gset . items)
  (define vh
    (fold
     (lambda (item vh)
       (define-values (add assoc)
	 (if (or (live-refr? item) (symbol? item))
	     (values vhash-consq vhash-assoc)
	     (values vhash-cons vhash-assq)))
       ;; Ensure it's unique to the set
       (if (assoc item vh)
	   vh
	   (add item #t vh)))
     vlist-null items))
  (_make-gset vh))

(define (gset-add set item)
  (define add
    (if (or (live-refr? item) (symbol? item))
	vhash-consq
	vhash-cons))
  (if (gset-member? set item)
      set
      (_make-gset (add item #t (_set-ht set)))))

(define (gset-remove set item)
  (define del
    (if (or (live-refr? item) (symbol? item))
	vhash-delq
	vhash-delete))
  (_make-gset (del item (_set-ht set))))

(define (gset-fold proc init set)
  (vhash-fold
   (lambda (key _val prev)
     (proc key prev))
   init
   (_set-ht set)))

(define (gset-length set)
  (vhash-fold
   (lambda (_k _v count)
     (1+ count))
   0
   (_set-ht set)))

(define (gset->list set)
  (vhash-fold
   (lambda (key _val prev)
     (cons key prev))
   '()
   (_set-ht set)))

(define (gset-member? set key)
  (define assoc
    (if (or (live-refr? key) symbol? key)
	vhash-assq
	vhash-assoc))
  
  (match (assoc key (_set-ht set))
    [(_val . #t) #t]
    [#f #f]))

(define (gset-for-each proc set)
  (vhash-fold
   (lambda (k v _p)
     (proc k v))
   #f
   (_set-ht set)))
