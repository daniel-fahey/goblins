;;; Copyright 2021 Christine Lemmer-Webber
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

(define-module (goblins utils simple-sealers)
  #:export (make-sealer-triplet)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu))

;; Simple sealers, speedy "secret cookie" version

(define-record-type <sealed>
  (make-sealed secret content name)
  _sealed?
  (secret sealed-secret)
  (content sealed-content)
  (name sealed-name))

(define (print-sealed sealed port)
  (write-string "<sealed" port)
  (when name
    (display ": " port)
    (display (sealed-name sealed) port))
  (write-char #\> port))

(set-record-type-printer! <sealed> print-sealed)

(define* (make-sealer-triplet #:optional name)
  (define secret (cons '*secret* '*id*))
  (define (seal obj)
    (make-sealed secret obj name))
  (define (sealed? maybe-sealed)
    (and (_sealed? maybe-sealed)
         (eq? (sealed-secret maybe-sealed) secret)))
  (define (unseal obj)
    (unless (sealed? obj)
      (if (_sealed? obj)
          (error "Wrong unsealer for object:" obj)
          (error "Not a sealed object:" obj)))
    (sealed-content obj))
  (values seal unseal sealed?))


;;; Here's the original, simple, "using srfi-9 records direcly"
;;; version, inspired by Rees's W7. However, the above "secret cookie"
;;; version is 20x faster.

#;(define* (make-sealer-triplet #:optional name)
  (define-record-type <seal>
    (seal val)
    sealed?
    (val unseal))
  (set-record-type-printer! 
   <seal>
   (lambda (record port)
     (if name
         (begin
           (display "<sealed: " port)
           (display name port)
           (display ">" port))
         (display "<sealed>" port))))
  (values seal unseal sealed?))

