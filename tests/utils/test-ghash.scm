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

;; Really presently built on top of vhashes.  Might be built on top
;; of something else, like fashes, in the future.


(define-module (tests utils test-ghash)
  #:use-module (goblins utils ghash)
  #:use-module (goblins core)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 hash-table))

(test-begin "test-ghash")

(define gh1 (ghash 'key1 'val1 'key2 'val2))

(test-equal 'val1 (ghash-ref gh1 'key1))
(test-equal 'val2 (ghash-ref gh1 'key2))
(test-equal #f (ghash-ref gh1 'nonsense))
(test-equal 'some-dflt (ghash-ref gh1 'nonsense 'some-dflt))

(test-equal '(("key1" "val1") ("key2" "val2"))
  (sort (ghash-fold
         (lambda (k v p)
           (cons (list (symbol->string k)
                       (symbol->string v))
                 p))
         '() gh1)
        (lambda (c1 c2)
          (string<? (car c1) (car c2)))))

(define gh2
  (ghash 'zelle 'pronk 'beep 'boop 'meep 'moop))
(test-equal 'moop (ghash-ref gh2 'meep))
(test-equal 'boop (ghash-ref gh2 'beep))
(test-equal 'pronk (ghash-ref gh2 'zelle))
(test-equal 3 (ghash-length gh2))
(test-equal #f (ghash-ref (ghash-remove gh2 'zelle) 'zelle))
(test-equal 2 (ghash-length (ghash-remove gh2 'zelle)))

(define gh3
  (hash-table->ghash (alist->hash-table '((foo . 1) (bar . 2)))))
(test-equal 1 (ghash-ref gh3 'foo))
(test-equal 2 (ghash-ref gh3 'bar))
(test-equal #f (ghash-ref gh3 'baz))
(test-equal 2 (ghash-length gh3))

(define am (make-actormap))
(define (^friendo bcom) (lambda () "I'm a friend"))
(define alice (actormap-spawn! am ^friendo))
(define bob (actormap-spawn! am ^friendo))
(define gh4 (ghash alice "alice" bob "bob"))
;; make sure refrs hash with eq?
(test-equal "alice" (ghash-ref gh4 alice))
(test-equal "bob" (ghash-ref gh4 bob))
(define carol (actormap-spawn! am ^friendo))
(define gh5 (ghash-set gh4 carol "carol"))
(test-equal "carol" (ghash-ref gh5 carol))
(define gh6 (ghash-set gh5 'meep "meep"))
(test-equal "meep" (ghash-ref gh6 'meep))

;; Sets
(define gs1
  (make-gset 1 2 3))
(test-equal 3 (gset-length gs1))
(test-equal '(1 2 3) (sort (gset->list gs1) <))
(test-assert (gset-member? gs1 1))
(test-assert (gset-member? gs1 2))
(test-assert (gset-member? gs1 3))

;; Add new number
(define gs2
  (gset-add gs1 4))
(test-equal 4 (gset-length gs2))
(test-equal '(1 2 3 4) (sort (gset->list gs2) <))
(test-assert (gset-member? gs2 4))

;; Try adding same number
(define gs3
  (gset-add gs2 4))
(test-equal 4 (gset-length gs3))
(test-equal '(1 2 3 4) (sort (gset->list gs3) <))

;; Try removing a number
(define gs4
  (gset-remove gs3 2))
(test-equal 3 (gset-length gs4))
(test-equal '(4 3 1) (sort (gset->list gs4) >))
(test-assert (not (gset-member? gs4 2)))

;; Try constructing a set with multiple of the same value
(define gs5
  (make-gset 1 2 3 4 4 5))
(test-equal 5 (gset-length gs5))
(test-equal '(1 2 3 4 5) (sort (gset->list gs5) <))

(test-end "test-ghash")
