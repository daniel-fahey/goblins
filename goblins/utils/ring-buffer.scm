;;; Copyright 2023 David Thompson
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

;;; Commentary:
;;
;; Ring buffer data structure.
;;
;;; Code:
(define-module (goblins utils ring-buffer)
  #:use-module (srfi srfi-9)
  #:export (make-ring-buffer
            ring-buffer?
            ring-buffer-empty?
            ring-buffer-full?
            ring-buffer-capacity
            ring-buffer-length
            ring-buffer-put!
            ring-buffer-get!
            ring-buffer-ref
            ring-buffer-resize!
            ring-buffer-clear!))

(define-record-type <ring-buffer>
  (%make-ring-buffer vector length head tail)
  ring-buffer?
  (vector ring-buffer-vector set-ring-buffer-vector!)
  (length ring-buffer-length set-ring-buffer-length!)
  (head ring-buffer-head set-ring-buffer-head!)
  (tail ring-buffer-tail set-ring-buffer-tail!))

(define (make-ring-buffer capacity)
  (%make-ring-buffer (make-vector capacity #f) 0 0 0))

(define (ring-buffer-capacity ring)
  (vector-length (ring-buffer-vector ring)))

(define (ring-buffer-empty? ring)
  (zero? (ring-buffer-length ring)))

(define (ring-buffer-full? ring)
  (= (ring-buffer-length ring)
     (ring-buffer-capacity ring)))

(define (ring-buffer-put! ring x)
  (let* ((head (ring-buffer-head ring))
         (tail (ring-buffer-tail ring))
         (l (ring-buffer-length ring))
         (v (ring-buffer-vector ring))
         (vl (vector-length v)))
    (vector-set! v tail x)
    (set-ring-buffer-length! ring (min (+ l 1) vl))
    (when (and (> l 0) (= head tail))
      (set-ring-buffer-head! ring (modulo (+ head 1) vl)))
    (set-ring-buffer-tail! ring (modulo (+ tail 1) vl))))

(define (ring-buffer-get! ring)
  (if (ring-buffer-empty? ring)
      (error "ring buffer empty" ring)
      (let* ((head (ring-buffer-head ring))
             (v (ring-buffer-vector ring))
             (result (vector-ref v head)))
        (vector-set! v head #f)
        (set-ring-buffer-head! ring (modulo (+ head 1) (vector-length v)))
        (set-ring-buffer-length! ring (- (ring-buffer-length ring) 1))
        result)))

(define (ring-buffer-ref ring i)
  (let ((l (ring-buffer-length ring))
        (v (ring-buffer-vector ring)))
    (if (>= i l)
        (error "ring buffer index out of bounds" i)
        (vector-ref v (modulo (+ (ring-buffer-head ring) i)
                              (vector-length v))))))

(define (ring-buffer-resize! ring capacity)
  (let* ((new-v (make-vector capacity #f))
         ;; The buffer capacity might be shrinking, in which case we
         ;; can only copy a subset of the buffer items.
         (l (ring-buffer-length ring))
         (n (min l (vector-length new-v)))
         (start (- l n)))
    (let ((i 0))
      (when (< i n)
        (vector-set! new-v i (ring-buffer-ref ring (+ start i)))))
    (set-ring-buffer-vector! ring new-v)
    (set-ring-buffer-length! ring n)
    (set-ring-buffer-head! ring 0)
    (set-ring-buffer-tail! ring 0)))

(define (ring-buffer-clear! ring)
  (let ((v (ring-buffer-vector ring)))
    (set-ring-buffer-head! ring 0)
    (set-ring-buffer-tail! ring 0)
    (set-ring-buffer-length! ring 0)
    (let loop ((i 0))
      (when (< i (vector-length v))
        (vector-set! v i #f)
        (loop (+ i 1))))))
