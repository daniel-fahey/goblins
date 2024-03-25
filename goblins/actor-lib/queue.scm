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

(define-module (goblins actor-lib queue)
  #:use-module (goblins core)
  #:use-module (goblins define-actor)
  #:use-module (goblins actor-lib methods)
  #:export (^queue queue-env))

;; Uses the approach from Purely Functional Data Structures by Chris Okasaki
(define-actor (^queue* bcom #:optional [length 0] [head '()] [tail '()])
  "Constructs a FIFO queue.

Methods:
`length': Returns the length of the queue
`empty?': Returns #t is empty, #f otherwise.
`enqueue val': Adds VAL to queue.
`dequeue': Removes and returns the oldest inserted value from the queue.

Type: -> Queue"
  (define empty? (zero? length))
  (methods
   ((length) length)
   ((empty?) empty?)
   ((enqueue val)
    (bcom (^queue* bcom (1+ length)
                   head (cons val tail))))
   ((dequeue)
    (when empty?
      (error "Queue is already empty!"))
    (cond
     ;; If the head is empty, we reverse the tail, taking
     ;; the first value from that and handing it to the user,
     ;; and update our behavior to use the rest of the reversed
     ;; tail as the new head.
     ((null? head)
      (let* ((h* (reverse tail))
             (val (car h*))
             (new-head (cdr h*)))
        (bcom (^queue* bcom
		       (1- length)
                       new-head '())
              val)))
     ;; Otherwise, just pull from the top of the head as the dequeued
     ;; value.
     (else
      (bcom (^queue* bcom
		     (1- length)
                     (cdr head) tail)
            (car head)))))))

(define (^queue bcom)
  (^queue* bcom))
(define (restore-queue _version length head tail)
  (spawn ^queue* length head tail))

(define queue-env
  (make-persistence-env
   `((((goblins actor-lib queue) ^queue) ,^queue ,restore-queue))))
