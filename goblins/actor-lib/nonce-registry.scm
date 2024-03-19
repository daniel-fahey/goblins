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

(define-module (goblins actor-lib nonce-registry)
  #:use-module (gcrypt random)
  #:use-module (gcrypt base64)
  #:use-module (goblins core)
  #:use-module (goblins ghash)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib define-actor)
  #:use-module (goblins utils assert-type)
  #:use-module (goblins utils crypto)
  #:export (spawn-nonce-registry-and-locator
	    nonce-registry-env))

(define (make-swiss-num)
  (gen-random-bv 32 %gcry-strong-random))

(define-actor (^nonce-registry bcom #:optional [ht ghash-null])
  (define* (register refr #:optional provided-swiss-num)
    (assert-type refr live-refr?)
    (let* ((swiss-num (or provided-swiss-num (make-swiss-num)))
           (new-ht (ghash-set ht swiss-num refr)))
      (bcom (^nonce-registry bcom new-ht) swiss-num)))
  (methods
   [register register]
   [fetch
    (case-lambda
      [(swiss-num)
       ;; TODO: Better errors when no swiss num
       (unless (ghash-has-key? ht swiss-num)
         (throw 'no-such-key
                (format #f "No object registered with swiss-num: ~a"
                        (url-base64-encode swiss-num))))
       (ghash-ref ht swiss-num)]
      [(swiss-num dflt)
       (ghash-ref ht swiss-num dflt)])]))

(define-actor (^nonce-locator bcom registry)
  (methods
   [(fetch swiss-num)
    ($ registry 'fetch swiss-num)]))

(define (spawn-nonce-registry-and-locator)
  "Return a new Nonce-Registry and Nonce-Locator.

A Nonce-Registry is an object containing Swiss numbers, unique IDs
which provide access to some capability analogously to a Swiss bank
account number. A Nonce-Locator is a proxy object granting access to
but not storage of Swiss numbers in its related Nonce-Registry.

Nonce-Registry Methods:
`register refr [provided-swiss-num]': Add REFR to the registry using
PROVIDED-SWISS-NUM or a newly-generated one; return the swiss-num.
`fetch swiss-num [dflt]': Return the object associated with SWISS-NUM if
it exists, or DFLT if it is provided and the SWISS-NUM is not registered.
If DFLT is not provided and SWISS-NUM is not registered, error.

Nonce-Locator Methods:
`fetch swiss-num': Return the object associated with SWISS-NUM.

Type: -> (Values Nonce-Registry Nonce-Locator)"
  (let* ((registry (spawn-named 'nonce-registry ^nonce-registry))
	 (locator (spawn-named 'nonce-locator ^nonce-locator registry)))
    (values registry locator)))

(define nonce-registry-env
  (make-persistence-env
   `((((goblins actor-lib nonce-registry) ^nonce-locator) ,^nonce-locator)
     (((goblins actor-lib nonce-registry) ^nonce-registry) ,^nonce-registry))))
