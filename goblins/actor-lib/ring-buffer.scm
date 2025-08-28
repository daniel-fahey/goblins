;;; Copyright 2025 Jessica Tallon
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

(define-module (goblins actor-lib ring-buffer)
  #:use-module (goblins)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib methods)
  #:export (^ring-buffer
            ring-buffer-env))

;; Implementation very much taken/inspired by (goblins utils ring-buffer)
(define-actor (^ring-buffer* bcom vec length capacity head tail)
  (define (ref index)
    ($ vec 'ref (modulo index capacity)))
  (methods
   [(empty?) (zero? length)]
   [(full?) (= length capacity)]
   [(capacity) capacity]
   [(length) length]
   [(put value)
    (let ((new-length (min (1+ length) capacity)))
      ($ vec 'set tail value)
      (bcom (^ring-buffer* bcom vec new-length capacity
                           (if (and (> length 0) (= head tail))
                               (modulo (1+ head) capacity)
                               head)
                           (modulo (1+ tail) capacity))))]
   [(take)
    (when (zero? length)
      (error "Ring buffer is empty"))
    (let ((result ($ vec 'ref head)))
      ($ vec 'set head #f)
      (bcom (^ring-buffer* bcom vec (1- length) capacity
                           (modulo (1+ head) capacity) tail)
            result))]
   [(ref index)
    (when (or (>= index length) (< index 0))
      (error "Ring buffer index out of bounds" index))
    (ref (+ head index))]
   [(head amount)
    (let lp ((read-head head)
             (amount-left (min amount length)))
      (if (zero? amount-left)
          '()
          (cons (ref read-head)
                (lp (1+ read-head) (1- amount-left)))))]
   [(tail amount)
    (define amount-to-read (min amount length))
    (let lp ((read-head (- tail amount-to-read))
             (amount-left amount-to-read))
      (if (zero? amount-left)
          '()
          (cons (ref read-head) (lp (1+ read-head) (1- amount-left)))))]
   [(resize new-capacity)
    (define new-vec (spawn ^vector new-capacity))
    ;; The buffer capacity might be shrinking, in which case we can only copy
    ;; a subset of the buffer items. In such a case, prefer newer values.
    (define new-length (min length new-capacity))
    (define start (- length new-length))
    (let lp ((index 0))
      (when (< index new-length)
        ($ new-vec 'set index (ref (+ head index start)))
        (lp (1+ index))))
    (let ((new-tail (modulo new-length new-capacity)))
      (bcom (^ring-buffer* bcom new-vec new-length new-capacity 0 new-tail)))]
   [(clear)
    ($ vec 'fill #f)
    (bcom (^ring-buffer* bcom vec 0 capacity 0 0))]))

(define (^ring-buffer bcom capacity)
  "Construct new ^ring-buffer actor with a capacity of @var{capacity}."
  (spawn ^ring-buffer* (spawn ^vector capacity) 0 capacity 0 0))

(define ring-buffer-env
  (make-persistence-env
   `((((goblins actor-lib ring-buffer) ^ring-buffer) ,^ring-buffer*))
   #:extends common-env))
