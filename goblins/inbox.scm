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

(define-module (goblins inbox)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  ;; #:use-module (srfi srfi-9)
  ;; #:use-module (ice-9 atomic)
  #:export (spawn-delivery-agent))

(define (spawn-delivery-agent)
  (define enq-ch (make-channel))
  (define deq-ch (make-channel))
  (define stop? (make-condition))
  (define back-queue (make-q))
  (define next-one #f)
  (define (start-vat-loop)
    (define keep-going? #t)
    ;; Incoming
    (define (enq-op)
      (wrap-operation (get-operation enq-ch)
                      (lambda (msg)
                        (if next-one
                            (enq! back-queue msg)
                            (set! next-one msg)))))
    ;; outgoing
    (define (deq-op)
      (wrap-operation
       ;; send it...
       (put-operation deq-ch next-one)
       ;; and pull next-one off the back-queue, if appropriate
       ;; (or indicate that there *is* no next-one...)
       (lambda _
         (if (q-empty? back-queue)
             (set! next-one #f)
             (set! next-one (deq! back-queue))))))
    (define (stop-op)
      (wrap-operation (wait-operation stop?)
                      (lambda (_) (set! keep-going? #f))))
    (while keep-going?
      (perform-operation
       (if next-one
           (choice-operation (deq-op) (enq-op) (stop-op))
           (choice-operation (enq-op) (stop-op))))))
  ;; boot it up!
  (spawn-fiber start-vat-loop)
  ;; return inbox enqueue/dequeue channels, as well as stop operation
  (values enq-ch deq-ch stop?))

