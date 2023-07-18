;;; Copyright 2023 Christine Lemmer-Webber
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

(define-module (tests utils base32)
  #:use-module (goblins utils base32)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 iconv)
  #:use-module (srfi srfi-64))

(test-begin "test-base32")

(test-equal "base32 encoding without padding v0"
  "jbswy3dpebrgc43fgmzccijb"
  (base32-encode (string->bytevector "Hello base32!!!" "UTF-8")
                 #:pad? #f))

(test-equal "base32 encoding without padding v1"
  "jbswy3dpebrgc43fgmzccii"
  (base32-encode (string->bytevector "Hello base32!!" "UTF-8")
                 #:pad? #f))

(test-equal "base32 encoding without padding v2"
  "jbswy3dpebrgc43fgmzcc"
  (base32-encode (string->bytevector "Hello base32!" "UTF-8")
                 #:pad? #f))

(test-equal "base32 encoding without padding v3"
  "jbswy3dpebrgc43fgmza"
  (base32-encode (string->bytevector "Hello base32" "UTF-8")
                 #:pad? #f))

(test-equal "base32 encoding without padding v4"
  "jbswy3dpmjqxgzjtgi"
  (base32-encode (string->bytevector "Hellobase32" "UTF-8")
                 #:pad? #f))

(test-equal "base32 encoding with padding v0"
  "jbswy3dpebrgc43fgmzccijb"
  (base32-encode (string->bytevector "Hello base32!!!" "UTF-8")
                 #:pad? #t))

(test-equal "base32 encoding with padding v1"
  "jbswy3dpebrgc43fgmzccii="
  (base32-encode (string->bytevector "Hello base32!!" "UTF-8")
                 #:pad? #t))

(test-equal "base32 encoding with padding v2"
  "jbswy3dpebrgc43fgmzcc==="
  (base32-encode (string->bytevector "Hello base32!" "UTF-8")
                 #:pad? #t))

(test-equal "base32 encoding with padding v3"
  "jbswy3dpebrgc43fgmza===="
  (base32-encode (string->bytevector "Hello base32" "UTF-8")
                 #:pad? #t))

(test-equal "base32 encoding with padding v4"
  "jbswy3dpmjqxgzjtgi======"
  (base32-encode (string->bytevector "Hellobase32" "UTF-8")
                 #:pad? #t))

(test-equal "base32 decoding with/out padding v0"
  "Hello base32!!!"
  (bytevector->string (base32-decode "jbswy3dpebrgc43fgmzccijb")
                      "UTF-8"))

(test-equal "base32 decoding without padding v1"
  "Hello base32!!"
  (bytevector->string (base32-decode "jbswy3dpebrgc43fgmzccii")
                      "UTF-8"))

(test-equal "base32 decoding without padding v2"
  "Hello base32!"
  (bytevector->string (base32-decode "jbswy3dpebrgc43fgmzcc")
                      "UTF-8"))

(test-equal "base32 decoding without padding v3"
  "Hello base32"
  (bytevector->string (base32-decode "jbswy3dpebrgc43fgmza")
                      "UTF-8"))

(test-equal "base32 decoding without padding v4"
  "Hellobase32"
  (bytevector->string (base32-decode "jbswy3dpmjqxgzjtgi")
                      "UTF-8"))

(test-equal "base32 decoding with padding v1"
  "Hello base32!!"
  (bytevector->string (base32-decode "jbswy3dpebrgc43fgmzccii=")
                      "UTF-8"))

(test-equal "base32 decoding with padding v2"
  "Hello base32!"
  (bytevector->string (base32-decode "jbswy3dpebrgc43fgmzcc===")
                      "UTF-8"))

(test-equal "base32 decoding with padding v3"
  "Hello base32"
  (bytevector->string (base32-decode "jbswy3dpebrgc43fgmza====")
                      "UTF-8"))

(test-equal "base32 decoding with padding v4"
  "Hellobase32"
  (bytevector->string (base32-decode "jbswy3dpmjqxgzjtgi======")
                      "UTF-8"))

(test-end "test-base32")
