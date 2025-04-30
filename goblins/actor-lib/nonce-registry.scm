;;; Copyright 2020-2021 Christine Lemmer-Webber
;;; Copyright 2023 Juliana Sims
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

(define-module (goblins actor-lib nonce-registry)
  #:use-module (goblins core)
  #:use-module (goblins define-actor)
  #:use-module (goblins utils ghash)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins utils assert-type)
  #:use-module (goblins utils crypto)
  #:use-module (goblins contrib base64)
  #:use-module (ice-9 match)
  #:use-module (scheme base) ;; bytevector-append
  #:export (spawn-nonce-registry-and-locator
            nonce-registry-env))

(define (make-swiss-num)
  (strong-random-bytes 32))

(define (refr->storable-id local-refr)
  "Convert a local-refr to a persistence object identifer."
  ;; If the nonce registry stored and restored by the persistence system then
  ;; local refrs would be woken up as promises for far local-refrs. This causes
  ;; problems for looking them up in the future as the refr and promise won't be
  ;; either equal or eq and we can't block all lookups until every promise is
  ;; resolved. The solution, convert them to persistence object identifiers so
  ;; that we can always compare them, even if the vat hasn't even woken up.
  (if (has-persistable-object-identifier? local-refr)
      (local-refr->persistable-object-identifier local-refr)
      local-refr))

(define-actor (^nonce-registry bcom
                               #:optional
                               [swiss-num->refr (make-ghash)]
                               [storable-id->swiss-num (make-ghash)]
                               [hash-algorithm 'sha256]
                               [salt (make-swiss-num)])
  #:frozen
  (define hash-func
    (match hash-algorithm
      ['sha256 sha256]))
  (define (hash value)
    (hash-func (bytevector-append value salt)))
  (define (register-refr-new refr provided-swiss-num)
    ;; If the refr is persistable, we want to store its persistable-id so that
    ;; we are able to lookup a refr even before that vat has been restored.
    ;; However if the refr is not on a persistable vat it cannot have a persistable
    ;; identifier and so we must fall back to using the refr itself. The refr
    ;; obviously won't have any persistability issues as it's not using the
    ;; persistence system.
    (let* ((storable-id (refr->storable-id refr))
           (new-swiss-num (or provided-swiss-num (make-swiss-num)))
           (hashed-swiss-num (hash new-swiss-num))
           (new-swiss-num->refr (ghash-set swiss-num->refr hashed-swiss-num refr))
           (new-storable-id->swiss-num
            (ghash-set storable-id->swiss-num storable-id new-swiss-num)))
      (bcom (^nonce-registry bcom new-swiss-num->refr
                             new-storable-id->swiss-num
                             hash-algorithm salt) new-swiss-num)))
  (define* (register refr #:optional provided-swiss-num)
    (assert-type refr live-refr?)
    (match (ghash-ref storable-id->swiss-num (refr->storable-id refr) #f)
      [#f (register-refr-new refr provided-swiss-num)]
      [swiss-num swiss-num]))
  (methods
   [register register]
   [fetch
    (case-lambda
      [(swiss-num)
       (let ((hashed-swiss-num (hash swiss-num)))
         ;; TODO: Better errors when no swiss num
         (unless (ghash-has-key? swiss-num->refr hashed-swiss-num)
           (throw 'no-such-key
                  (format #f "No object registered with swiss-num: ~a"
                          (base64-encode swiss-num
                                         #:alphabet base64-url-alphabet
                                         #:padding? #f))))
         (ghash-ref swiss-num->refr hashed-swiss-num))]
      [(swiss-num dflt)
       (let ((hashed-swiss-num (hash swiss-num)))
         (ghash-ref swiss-num->refr hashed-swiss-num dflt))])]))

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
