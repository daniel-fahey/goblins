;;; Copyright 2020-2021 Christine Lemmer-Webber
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
  #:use-module (goblins ghash)
  #:use-module (goblins actor-lib methods)
  #:export (^seteq
            ^ghash))

;; And the rest, eventually...
(define (^seteq bcom . initial)
  (let next ((vh (fold (lambda (i vh)
                         (vhash-consq i #t vh))
                       vlist-null
                       initial)))
    (methods
     [(add val)
      (bcom (next (vhash-consq val #t vh)))]
     [(remove val)
      (bcom (next (vhash-delq val vh)))]
     [(member? val)
      (and (vhash-assq val vh) #t)]
     [(as-list)
      (vhash-fold (lambda (k v lst)
                    (cons k lst))
                  (list)
                  vh)])))

(define* (^ghash bcom #:optional [ht ghash-null])
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
