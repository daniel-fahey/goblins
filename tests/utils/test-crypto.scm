;;; Copyright 2024 Jessica Tallon
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

(define-module (tests utils crypto)
  #:use-module (goblins utils crypto)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors))

(test-begin "test-crypto")

(define test-bv
  (string->utf8
   "This is a random test string!"))

(test-equal "Check SHA256d produces two sha256 hashes"
  #vu8(148 162 203 11 96 44 54 106 36 50 102 220 186 141
       30 74 170 18 180 25 129 226 250 41 124 197 202 132
       98 50 63 155)
  (sha256d test-bv))

(define keypair
  (generate-key-pair))

(test-assert "key-pair->public-key returns a S-expression formatted key"
  (match (key-pair->public-key keypair)
    [(public-key (ecc (curve Ed25519) (flags eddsa) (q data-goes-here)))
     #t]
    [something-else #f]))

(define sig
  (sign test-bv (key-pair->private-key keypair)))

(test-assert "Check signatured returned passes signature-sexp?"
  (signature-sexp? sig))

(test-assert "Check signature-sexp? does not return true for public-key"
  (not (signature-sexp? (key-pair->public-key keypair))))

(test-assert "Check verify function returns true for the signature we just made"
  (verify (captp-signature->crypto-signature sig)
          test-bv
          (captp-public-key->crypto-public-key
           (key-pair->public-key keypair))))

(test-end "test-crypto")
