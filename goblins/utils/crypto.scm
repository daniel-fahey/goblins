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
  #:use-module (gcrypt hash)
  #:use-module ((gcrypt pk-crypto) #:prefix gcrypt:pk-crypto:)
  #:use-module (gcrypt random)
  #:use-module (gcrypt base64)
  #:use-module (rnrs bytevectors)
  #:export (sha256d
            strong-random-bytes
            url-base64-encode
            url-base64-decode
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

(define (pad-b64-string b64-str)
  (define str-len (string-length b64-str))
  (define mod (modulo str-len 4))
  (if (zero? mod)
      b64-str
      (let ((padme (- 4 mod)))
        (string-append b64-str
                       (make-string padme #\=)))))

(define (url-base64-encode bv)
  (base64-encode bv 0 (bytevector-length bv) #f #t base64url-alphabet))

(define (url-base64-decode encoded-b64)
  (base64-decode (pad-b64-string encoded-b64) base64url-alphabet))

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
