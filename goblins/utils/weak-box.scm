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

;; Is this an acceptable way to define a weak box?
;; ... hell if I know, but probably?

(define-module (goblins utils weak-box)
  #:use-module (ice-9 weak-vector)
  #:use-module (srfi srfi-9)
  #:export (make-weak-box
            weak-box-value
            weak-box?))

(define-record-type <weak-box>
  (_make-weak-box wvec)
  weak-box?
  (wvec weak-box-wvec))

(define (make-weak-box val)
  (_make-weak-box (make-weak-vector 1 val)))

(define (weak-box-value weak-box)
  (weak-vector-ref (weak-box-wvec weak-box) 0))
