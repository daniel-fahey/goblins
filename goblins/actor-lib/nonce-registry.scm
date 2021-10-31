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

(define-module (goblins actor-lib nonce-registry)
  #:use-module (gcrypt random)
  #:use-module (gcrypt base64)
  #:use-module (goblins core)
  #:use-module (goblins ghash)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins utils assert-type)
  #:use-module (goblins utils crypto-stuff)
  #:export (spawn-nonce-registry-locator-pair
            spawn-nonce-registry-locator-values))

(define (make-swiss-num)
  (gen-random-bv 32 %gcry-strong-random))

(define (^nonce-registry bcom)
  (let next-self ([ht ghash-null])
    (methods
     [(register refr)
      (assert-type refr live-refr?)
      (let* ((swiss-num (make-swiss-num))
             (new-ht (ghash-set ht swiss-num refr)))
        (bcom (next-self new-ht)
              swiss-num))]
     [fetch
      (case-lambda
        [(swiss-num)
         ;; TODO: Better errors when no swiss num
         (unless (ghash-has-key? ht swiss-num)
           (throw 'no-such-key
                  (format #f "No object registered with swiss-num: ~a"
                          (url-base64-encode swiss-num))))
         (hash-ref ht swiss-num)]
        [(swiss-num dflt)
         (hash-ref ht swiss-num dflt)])])))

(define (spawn-nonce-registry-locator-pair)
  (define registry
    (spawn ^nonce-registry))
  (define (^nonce-locator bcom)
    (methods
     [(fetch swiss-num)
      ($ registry 'fetch swiss-num)]))
  (define locator
    (spawn ^nonce-locator))
  (values registry locator))
