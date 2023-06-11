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

;; Design inspired by Rees's W7

(define* (make-sealer-triplet #:optional name)
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
