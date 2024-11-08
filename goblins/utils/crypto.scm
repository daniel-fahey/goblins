;;; Copyright 2021 Christine Lemmer-Webber
;;; Copyright 2022 Jessica Tallon
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

(define-module (goblins utils crypto)
  #:use-module (goblins contrib base64)
  #:use-module (gcrypt hash)
  #:use-module ((gcrypt pk-crypto) #:prefix gcrypt:pk-crypto:)
  #:use-module (gcrypt random)
  #:use-module (rnrs bytevectors)
  #:export (sha256d
            strong-random-bytes
            generate-key-pair
            key-pair->public-key
            key-pair->private-key
            sign
            verify
            signature-sexp?))

(define (sha256d input)
  (sha256 (sha256 input)))

(define (strong-random-bytes byte-size)
  (gen-random-bv 32 %gcry-strong-random))

(define (data->canonical-sexp data)
  (gcrypt:pk-crypto:sexp->canonical-sexp
   `(data (flags eddsa) (hash-algo sha512)
          (value ,data))))

(define (generate-key-pair)
  (gcrypt:pk-crypto:generate-key
   (gcrypt:pk-crypto:sexp->canonical-sexp
    '(genkey (eddsa (curve Ed25519) (flags eddsa))))))

(define (key-pair->private-key keypair)
  (gcrypt:pk-crypto:find-sexp-token keypair 'private-key))
(define (key-pair->public-key keypair)
  (gcrypt:pk-crypto:find-sexp-token keypair 'public-key))

(define (sign data private-key)
  (gcrypt:pk-crypto:canonical-sexp->sexp
   (gcrypt:pk-crypto:sign
    (data->canonical-sexp data)
    private-key)))

(define (verify signature data public-key)
  (gcrypt:pk-crypto:verify signature (data->canonical-sexp data) public-key))

(define (signature-sexp? maybe-signature)
  (and (list? maybe-signature)
       (eq? (car maybe-signature) 'sig-val)))
