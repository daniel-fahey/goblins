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
          captp-signature->crypto-signature
          private-key->data
          data->private-key)

  (import (goblins utils js-data)
          (guile)
          (ice-9 match)
          (except (rnrs bytevectors) bytevector-copy)
          (only (scheme base) bytevector-append bytevector-copy))

  (cond-expand
   (hoot
    (import (hoot ffi)
            (fibers promises)))
   (guile
    (import (ice-9 receive)
            (gnutls))))

  (begin
    (define (sha256d input)
      (sha256 (sha256 input)))

    (define (signature-sexp? maybe-signature)
      (and (list? maybe-signature)
           (eq? (car maybe-signature) 'sig-val)))

    (define (strong-random-bytes byte-size)
      (cond-expand
       (guile (gnutls-random random-level/key byte-size))
       (hoot (gen-random-bv byte-size))))

    ;; Some guile or hoot specific functions which need to be define.
    (cond-expand
     (guile
      (define (sha256 input)
        (hash-direct digest/sha256 input)))
     (hoot
      ;; Import FFI Stuff
      ;; ================

      ;; Uint8Array -> Uint8Array
      (define-foreign random-values
        "crypto" "randomValues"
        i32 -> (ref extern))
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

      ;; Hashing
      ;; =======
      (define (sha256 input)
        (uint8-array->bytevector
         (await (digest "SHA-256"
                        (if (bytevector? input)
                            (bytevector->uint8-array input)
                            input)))))

      ;; Gcrypt like functions
      (define* (gen-random-bv length)
        "Return a bytevector of length @var{length} filled with random
bytes.

Type: (Optional UnsignedInteger) -> Bytevector"
        (uint8-array->bytevector
         (random-values length)))

      (define (export-key key)
        "Export @var{key} to its raw binary format

Type: CryptoKey -> ByteVector"
        (uint8-array->bytevector (await (%export-key key))))))

    (define (private-key->data pk)
      "Return exported private key data"
      (cond-expand
       (guile
        (define-values (crv x y k)
          (private-key-export-raw-ecc pk))
        (define algorithm
          (ecc-curve->pk-algorithm crv))
        (list (pk-algorithm->string algorithm) x y k))
       (hoot
        (error "Not supported on hoot"))))

    (define (data->private-key data)
      (cond-expand
       (guile
        (match data
          [("EdDSA (Ed25519)" x y k)
           (import-raw-ecc-private-key ecc-curve/ed25519 x y k)]
          [something-else
           (error "Do not know how to import key" data)]))
       (hoot
        (error "Not supported on hoot"))))

    (define (generate-key-pair)
      (cond-expand
       (guile
        (generate-private-key pk-algorithm/eddsa-ed25519 ecc-curve/ed25519))
       (hoot
        (await (generate-ed25519-key-pair)))))

    (define (key-pair->private-key keypair)
      "Return the private key of the Ed25519 @var{key-pair}

Type: CryptoKeyPair -> CryptoKey"
      (cond-expand
       (hoot (extract-private-key keypair))
       (guile keypair)))

    (define (key-pair->public-key keypair)
      "Return the public key of the Ed25519 @var{key-pair}

Type: CryptoKeyPair -> S-Expression"
      `(public-key (ecc (curve Ed25519)
                        (flags eddsa)
                        (q ,(cond-expand
                             (guile
                              (receive (_crv x _y _k)
                                  (private-key-export-raw-ecc keypair)
                                x))
                             (hoot
                              (export-key
                               (extract-public-key keypair))))))))

    (define (sign data private-key)
      "Sign @var{data} using Ed25519 @var{private-key}

Type: Bytevector CryptoKey -> List"
      (let ((sig-bv
             (cond-expand
              (guile
               (private-key-sign-data private-key sign-algorithm/eddsa-ed25519 data '()))
              (hoot
               (uint8-array->bytevector
                (await
                 (sign-ed25519 (bytevector->uint8-array data)
                               private-key)))))))
        `(sig-val (eddsa (r ,(bytevector-copy sig-bv 0 32))
                         (s ,(bytevector-copy sig-bv 32))))))

    (define (verify signature data public-key)
      "Verify @var{signature} of @var{data} using Ed25519 @var{public-key}

Type: CryptoSignature Bytevector CryptoKey -> Boolean"
      (cond-expand
       (guile
        (catch 'gnutls-error
               (lambda ()
                 (public-key-verify-data
                  public-key
                  sign-algorithm/eddsa-ed25519
                  data
                  signature))
               (lambda _ #f)))
       (hoot
        (await (verify-ed25519 signature
                               (bytevector->uint8-array data)
                               public-key)))))

    (define (captp-public-key->crypto-public-key key)
      "Convert @var{key} from its CapTP wire format to its internal format

Type: S-Expression -> CryptoKey"
      (match key
        (`(public-key (ecc (curve Ed25519)
                           (flags eddsa)
                           (q ,data)))
         (cond-expand
          (guile
           (import-raw-ecc-public-key ecc-curve/ed25519 data #vu8()))
          (hoot
           (await (import-public-key (bytevector->uint8-array data))))))))

    (define (captp-signature->crypto-signature signature)
      "Convert @var{signature} from its CapTP wire format to its internal format

Type: List -> CryptSignature"
      (let ((bytes
             (match signature
               (`(sig-val (eddsa (r ,r) (s ,s)))
                (bytevector-append r s)))))
        (cond-expand
         (guile bytes)
         (hoot
          (bytevector->uint8-array bytes)))))))
