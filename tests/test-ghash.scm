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


(define-module (test test-ghash)
  #:use-module (goblins ghash)
  #:use-module (goblins core)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 hash-table))
  
(test-begin "test-ghash")

(define gh1 (make-ghash 'key1 'val1 'key2 'val2))

(test-equal (ghash-ref gh1 'key1) 'val1)
(test-equal (ghash-ref gh1 'key2) 'val2)
(test-equal (ghash-ref gh1 'nonsense) #f)
(test-equal (ghash-ref gh1 'nonsense 'some-dflt) 'some-dflt)

(test-equal '(("key1" "val1") ("key2" "val2"))
  (ghash-fold
   (lambda (k v p)
     (cons (list (symbol->string k)
                 (symbol->string v))
           p))
   '()
   gh1))

(define gh2
  (ghash-set (ghash-set (ghash-set ghash-null 'meep 'moop)
                        'beep 'boop)
             'zelle 'pronk))
(test-equal (ghash-ref gh2 'meep) 'moop)
(test-equal (ghash-ref gh2 'beep) 'boop)
(test-equal (ghash-ref gh2 'zelle) 'pronk)
(test-equal (ghash-length gh2) 3)
(test-equal (ghash-ref (ghash-remove gh2 'zelle) 'zelle) #f)
(test-equal (ghash-length (ghash-remove gh2 'zelle)) 2)

(define gh3
  (hash-table->ghash (alist->hash-table '((foo . 1) (bar . 2)))))
(test-equal (ghash-ref gh3 'foo) 1)
(test-equal (ghash-ref gh3 'bar) 2)
(test-equal (ghash-ref gh3 'baz) #f)
(test-equal (ghash-length gh3) 2)

(define am (make-actormap))
(define (^friendo bcom) (lambda () "I'm a friend"))
(define alice (actormap-spawn! am ^friendo))
(define bob (actormap-spawn! am ^friendo))
(define gh4 (make-ghash alice "alice" bob "bob"))
;; make sure refrs hash with eq?
(test-eq (ghash-ref gh4 alice) "alice")
(test-eq (ghash-ref gh4 bob) "bob")

(test-end "test-ghash")
