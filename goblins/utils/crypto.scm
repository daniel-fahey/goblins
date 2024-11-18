;;; Copyright 2021 Christine Lemmer-Webber
;;; Copyright 2022-2024 Jessica Tallon
;;; Copyright 2024 Juliana Sims <juli@incana.org>
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

;; use-modules does not support cond-expand, so define-library must be used.
(define-library (goblins utils crypto)
  (export sha256d
          sha256
          strong-random-bytes
          generate-key-pair
          key-pair->public-key
          key-pair->private-key
          sign
          verify
          signature-sexp?
          captp-public-key->crypto-public-key
          captp-signature->crypto-signature)

  (cond-expand
   (hoot
    (import (guile)
            (hoot ffi)
            (ice-9 match)
            (fibers promises)
            (except (rnrs bytevectors) bytevector-copy)
            (only (scheme base) bytevector-append bytevector-copy)))
   (guile
    (import (guile)
            (ice-9 match)
            (rnrs bytevectors)
            (prefix (gcrypt pk-crypto) gcrypt:pk-crypto:)
            (gcrypt hash)
            (gcrypt random))))

  (begin
    (define (sha256d input)
      (sha256 (sha256 input)))

    (define (signature-sexp? maybe-signature)
      (and (list? maybe-signature)
           (eq? (car maybe-signature) 'sig-val)))

    (define (strong-random-bytes byte-size)
      (cond-expand
       (guile (gen-random-bv byte-size %gcry-strong-random))
       (hoot (gen-random-bv byte-size))))

    ;; Some guile or hoot specific functions which need to be define.
    (cond-expand
     (guile
      (define (data->canonical-sexp data)
        (gcrypt:pk-crypto:sexp->canonical-sexp
         `(data (flags eddsa) (hash-algo sha512)
           (value ,data)))))
     (hoot
      ;; Import FFI Stuff
      ;; ================

      ;; length -> Uint8Array
      (define-foreign make-uint8array
        "typedArray" "makeUint8Array" i32 -> (ref extern))
      ;; Uint8Array -> length
      (define-foreign uint8array-length
        "typedArray" "Uint8ArrayLength" (ref extern) -> i32)
      ;; Uint8Array, idx -> byte
      (define-foreign uint8array-ref
        "typedArray" "Uint8ArrayRef" (ref extern) i32 -> i32)
      ;; Uint8Array, idx, byte -> ()
      (define-foreign uint8array-set!
        "typedArray" "Uint8ArraySet" (ref extern) i32 i32 -> none)
      ;; Uint8Array -> Uint8Array
      (define-foreign get-random-values!
        "crypto" "getRandomValues"
        (ref extern) -> (ref extern))
      ;; String TypedArray -> (Promise Uint8Array)
      (define-foreign digest
        "crypto" "digest"
        (ref string) (ref extern) -> (ref extern))
      ;; -> (Promise CryptoKeyPair)
      (define-foreign generate-ed25519-key-pair
        "crypto" "generateEd25519KeyPair"
        -> (ref extern))
      ;; CryptoKeyPair -> CryptoKey
      (define-foreign extract-public-key
        "crypto" "keyPairPublicKey"
        (ref extern) -> (ref extern))
      ;; CryptoKeyPair -> CryptoKey
      (define-foreign extract-private-key
        "crypto" "keyPairPrivateKey"
        (ref extern) -> (ref extern))
      ;; CryptoKey -> (Promise Uint8Array)
      (define-foreign %export-key
        "crypto" "exportKey"
        (ref extern) -> (ref extern))
      ;; Uint8Array Uint8Array CryptoKey -> (Promise Boolean)
      (define-foreign verify-ed25519
        "crypto" "verifyEd25519"
        (ref extern) (ref extern) (ref extern) -> (ref extern))
      ;; Uint8Array -> (Promise CryptoKey)
      (define-foreign import-public-key
        "crypto" "importPublicKey"
        (ref extern) -> (ref extern))
      ;; Uint8Array CryptoKey -> (Promise Uint8Array)
      (define-foreign sign-ed25519
        "crypto" "signEd25519"
        (ref extern) (ref extern) -> (ref extern))

      ;; Some helpers
      ;; ============
      (define-syntax-rule (convert-arrays val len-proc make-proc set-proc! ref-proc)
        (let* ((len (len-proc val))
               (new-val (make-proc len)))
          (do ((i 0 (1+ i)))
              ((>= i len) new-val)
            (set-proc! new-val i (ref-proc val i)))))

      (define (uint8array->bytevector u8a)
        "Convert @var{u8a} to a Bytevector

Type: Uint8Array -> Bytevector"
        (convert-arrays u8a uint8array-length make-bytevector
                        bytevector-u8-set! uint8array-ref))
      (define (bytevector->uint8array bv)
        "Convert @var{bv} to a Uint8Array

Type: Bytevector -> Uint8Array"
        (convert-arrays bv bytevector-length make-uint8array
                        uint8array-set! bytevector-u8-ref))

      ;; Hashing
      ;; =======
      (define (sha256 input)
        (uint8array->bytevector
         (await (digest "SHA-256"
                        (if (bytevector? input)
                            (bytevector->uint8array input)
                            input)))))

      ;; Gcrypt like functions
      (define* (gen-random-bv #:optional (bv-length 50))
        "Generate a random Bytevector of @var{bv-length}

Type: (Optional UnsignedInteger) -> Bytevector"
        (uint8array->bytevector (get-random-values! (make-uint8array bv-length))))

      (define (export-key key)
        "Export @var{key} to its raw binary format

Type: CryptoKey -> ByteVector"
        (uint8array->bytevector (await (%export-key key))))))

    (define (generate-key-pair)
      (cond-expand
       (guile
        (gcrypt:pk-crypto:generate-key
         (gcrypt:pk-crypto:sexp->canonical-sexp
          '(genkey (eddsa (curve Ed25519) (flags eddsa))))))
       (hoot
        (await (generate-ed25519-key-pair)))))

    (define (key-pair->private-key keypair)
      "Return the private key of the Ed25519 @var{key-pair}

Type: CryptoKeyPair -> CryptoKey"
      (cond-expand
       (hoot (extract-private-key keypair))
       (guile
        (gcrypt:pk-crypto:find-sexp-token keypair 'private-key))))

    (define (key-pair->public-key keypair)
      "Return the public key of the Ed25519 @var{key-pair}

Type: CryptoKeyPair -> S-Expression"
      (cond-expand
       (guile
        (gcrypt:pk-crypto:canonical-sexp->sexp
         (gcrypt:pk-crypto:find-sexp-token keypair 'public-key)))
       (hoot
        `(public-key (ecc (curve Ed25519)
                          (flags eddsa)
                          (q ,(export-key
                               (extract-public-key keypair))))))))

    (define (sign data private-key)
      "Sign @var{data} using Ed25519 @var{private-key}

Type: Bytevector CryptoKey -> List"
      (cond-expand
       (guile
        (gcrypt:pk-crypto:canonical-sexp->sexp
         (gcrypt:pk-crypto:sign
          (data->canonical-sexp data)
          private-key)))
       (hoot
        (let ((sig-bv (uint8array->bytevector
                       (await (sign-ed25519 (bytevector->uint8array data) private-key)))))
          `(sig-val (eddsa (r ,(bytevector-copy sig-bv 0 32))
                           (s ,(bytevector-copy sig-bv 32))))))))

    (define (verify signature data public-key)
      "Verify @var{signature} of @var{data} using Ed25519 @var{public-key}

Type: Uint8Array Bytevector CryptoKey -> Boolean"
      (cond-expand
       (guile
        (gcrypt:pk-crypto:verify signature (data->canonical-sexp data) public-key))
       (hoot
        (await (verify-ed25519 signature
                               (bytevector->uint8array data)
                               public-key)))))

    (define (captp-public-key->crypto-public-key key)
      "Convert @var{key} from its CapTP wire format to its internal format

Type: S-Expression -> CryptoKey"
      (cond-expand
       (guile
        (gcrypt:pk-crypto:sexp->canonical-sexp key))
       (hoot
        (match key
          (`(public-key (ecc (curve Ed25519)
                             (flags eddsa)
                             (q ,data)))
           (await (import-public-key (bytevector->uint8array data))))))))

    (define (captp-signature->crypto-signature signature)
      "Convert @var{signature} from its CapTP wire format to its internal format

Type: List -> Uint8Array"
      (cond-expand
       (guile
        (gcrypt:pk-crypto:sexp->canonical-sexp signature))
       (hoot
        (match signature
          (`(sig-val (eddsa (r ,r) (s ,s)))
           (bytevector->uint8array (bytevector-append r s)))))))))
