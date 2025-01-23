;;; Copyright 2024 Juliana Sims <juli@incana.org>
;;; Copyright 2025 David Thompson <dave@spritely.institute>
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

;; Used in Hoot programs to convert between Scheme bytevectors and
;; JavaScript Uint8Arrays/ArrayBuffers.  The Guile implementation is
;; just there so we can import this module without a cond-expand.
(define-library (goblins utils js-data)
  (export bytevector->uint8-array
          uint8-array->bytevector
          array-buffer->bytevector)
  (import (only (ice-9 exceptions)
                raise-exception
                make-exception
                make-exception-with-irritants
                make-exception-with-message
                make-exception-with-origin)
          (scheme base))
  (cond-expand
   (guile)
   (hoot (import (hoot ffi))))
  (begin
    (cond-expand
     (guile
      (define-syntax define-unavailable
        (syntax-rules ()
          ((_ name arg ...)
           (define (name arg ...)
             (raise-exception
              (make-exception
               (make-exception-with-message "not available on Guile VM")
               (make-exception-with-origin 'name)
               (make-exception-with-irritants (list arg ...))))))))
      (define-unavailable bytevector->uint-array bv)
      (define-unavailable uint8-array->bytevector array)
      (define-unavailable array-buffer->bytevector buffer))
     (hoot
      (define-foreign make-uint8-array
        "uint8Array" "new"
        i32 -> (ref extern))
      (define-foreign array-buffer->uint8-array
        "uint8Array" "fromArrayBuffer"
        (ref extern) -> (ref extern))
      (define-foreign uint8-array-ref
        "uint8Array" "ref"
        (ref extern) i32 -> i32)
      (define-foreign uint8-array-set!
        "uint8Array" "set"
        (ref extern) i32 i32 -> none)
      (define-foreign uint8-array-length
        "uint8Array" "length"
        (ref null extern) -> i32)
      (define (uint8-array->bytevector array)
        (let* ((length (uint8-array-length array))
               (bv (make-bytevector length)))
          (do ((i 0 (+ i 1)))
              ((= i length))
            (bytevector-u8-set! bv i (uint8-array-ref array i)))
          bv))
      (define (bytevector->uint8-array bv)
        (let* ((length (bytevector-length bv))
               (array (make-uint8-array length)))
          (do ((i 0 (+ i 1)))
              ((= i length))
            (uint8-array-set! array i (bytevector-u8-ref bv i)))
          array))
      (define (array-buffer->bytevector buffer)
        (uint8-array->bytevector
         (array-buffer->uint8-array buffer)))))))
