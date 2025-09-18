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

(define-module (goblins utils bytes-stuff)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 iconv)
  #:export (bytes-append
            bytes<?
            string->bytes/latin-1
            string->bytes/utf-8
            bytes->string/utf-8
            bytes))

(define (bytes-append . bvs)
  (define new-bv-len
    (fold
     (lambda (x prev)
       (+ (bytevector-length x) prev))
     0 bvs))
  (define new-bv
    (make-bytevector new-bv-len))
  (let lp ([cur-pos 0]
           [bvs bvs])
    (match bvs
      [(this-bv . next-bvs)
       (define this-bv-len
         (bytevector-length this-bv))
       (bytevector-copy! this-bv 0 new-bv cur-pos this-bv-len)
       ;; move onto next bytevector
       (lp (+ cur-pos this-bv-len)
           next-bvs)]
      ['() 'done]))
  new-bv)

(define (bytes<? bstr1 bstr2)
  (define bstr1-len
    (bytevector-length bstr1))
  (define bstr2-len
    (bytevector-length bstr2))
  (let lp ([pos 0])
    (cond
     ;; we've reached the end of both and they're the same bytestring
     ;; but this isn't <=?
     [(and (eqv? bstr1-len pos)
           (eqv? bstr2-len pos))
      #f]
     ;; we've reached the end of bstr1 but not bstr2, so yes it's less
     [(eqv? bstr1-len pos)
      #t]
     ;; we've reached the end of bstr2 but not bstr1, so no
     [(eqv? bstr2-len pos)
      #f]
     ;; ok, time to compare bytes
     [else
      (let ([bstr1-byte (bytevector-u8-ref bstr1 pos)]
            [bstr2-byte (bytevector-u8-ref bstr2 pos)])
        (if (eqv? bstr1-byte bstr2-byte)
            ;; they're the same, so loop
            (lp (1+ pos))
            ;; otherwise, just compare numbers
            (< bstr1-byte bstr2-byte)))])))

(define (string->bytes/latin-1 str)
  (string->bytevector str "ISO-8859-1"))
(define (string->bytes/utf-8 str)
  (string->bytevector str "UTF-8"))
(define (bytes->string/utf-8 bstr)
  (bytevector->string bstr "UTF-8"))

;; alias for simplicity
(define bytes string->bytes/latin-1)
