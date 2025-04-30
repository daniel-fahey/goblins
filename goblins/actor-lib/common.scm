;;; Copyright 2020-2021 Christine Lemmer-Webber
;;; Copyright 2023 Juliana Sims
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

(define-module (goblins actor-lib common)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (goblins core)
  #:use-module (goblins define-actor)
  #:use-module (goblins ghash)
  #:use-module (goblins actor-lib methods)
  #:export (^seteq
            ^ghash
            common-env))

;; And the rest, eventually...
(define (^seteq bcom . initial)
  "Construct an actor representing a set where identity is compared
with `eq?'.

Methods:
`add val': Add VAL to the set.
`remove val': Remove VAL from the set.
`member? val': Return #t if VAL is in the set, else #f.
`as-list': Return the set as a cons list."
  (define (seteq vh)
    (define main-beh
      (methods
       [(add val)
        (bcom (seteq (vhash-consq val #t vh)))]
       [(remove val)
        (bcom (seteq (vhash-delq val vh)))]
       [(member? val)
        (and (vhash-assq val vh) #t)]
       [(as-list)
        (vhash-fold (lambda (k v lst)
                      (cons k lst))
                    (list)
                    vh)]))
    (define (self-portrait)
      (main-beh 'as-list))
    (portraitize main-beh self-portrait))
  (define vh
    (fold (lambda (i vh)
            (vhash-consq i #t vh))
          vlist-null
          initial))
  (seteq vh))

(define-actor (^ghash bcom #:optional [ht (make-ghash)])
  "Construct an actor providing a transactional interface to (goblins ghash),
a hashmap using `eq?' for refrs and `equal?' for everything else.

Methods:
`ref key [dflt]': Return the value associated with KEY or DFLT if it is not found.
`set key val': Associate KEY with VAL.
`has-key? key': Return #t if KEY is in the hashtable, else #f.
`remove key': Delete KEY and its associated value.
`data': Return the underlying hashtable."
  #:frozen
  (methods
   [ref
    (case-lambda
      [(key)
       (ghash-ref ht key)]
      [(key dflt)
       (ghash-ref ht key dflt)])]
   [(set key val)
    (bcom (^ghash bcom (ghash-set ht key val)))]
   [(has-key? key)
    (ghash-has-key? ht key)]
   [(remove key)
    (bcom (^ghash bcom (ghash-remove ht key)))]
   [(data) ht]))

(define common-env
  (make-persistence-env
   `((((goblins actor-lib common) ^ghash) ,^ghash)
     (((goblins actor-lib common) ^seteq) ,^seteq))))
