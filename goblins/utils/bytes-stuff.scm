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
  #:export (bytes-append))

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
