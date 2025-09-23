;;; Copyright 2024 Jessica Tallon
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

(define-module (goblins actor-lib inbox)
  #:use-module (goblins)
  #:use-module (goblins actor-lib ward)
  #:use-module (goblins actor-lib queue)
  #:use-module (srfi srfi-11)
  #:export (spawn-inbox inbox-env))

(define-actor (^inbox bcom read-warden write-warden stop-warden
                      #:optional
                      [messages (spawn ^queue)]
                      [pending (spawn ^queue)]
                      [stopped? #f])
  #:portrait (lambda () (list read-warden write-warden stop-warden messages stopped?))
  #:restore (lambda (_version read-warden write-warden stop-warden messages stopped?)
              (spawn ^inbox read-warden write-warden stop-warden
                     messages (spawn ^queue) stopped?))

  (define (read-beh)
    (if ($ messages 'empty?)
        ;; We have no messages to give, give a promise to a message
        (let-values (((vow resolver) (spawn-promise-and-resolver)))
          ($ pending 'enqueue resolver)
          vow)
        ($ messages 'dequeue)))

  (define (write-beh message)
    (if ($ pending 'empty?)
        ($ messages 'enqueue message)
        (let ((waiting-resolver ($ pending 'dequeue)))
          ($ waiting-resolver 'fulfill message))))

  (define (defunct . _args)
    (error "Inbox is closed"))

  (define (stop-beh)
    (while (not ($ pending 'empty?))
      (let ((waiting-resolver ($ pending 'dequeue)))
        ($ waiting-resolver 'break 'inbox-closed)))
    (bcom (^inbox bcom read-warden write-warden stop-warden #f #f #t)))

  (if stopped?
      defunct
      (let* ((warded-stop (ward stop-warden stop-beh))
             (warded-write-stop (ward write-warden write-beh #:extends warded-stop))
             (warded-write-stop-read (ward read-warden read-beh #:extends warded-write-stop)))
        warded-write-stop-read)))

(define-actor (^inbox-op _bcom incanter inbox)
  (lambda args
    (apply $ incanter inbox args)))

(define (spawn-inbox)
  (define-values (read-warden read-incanter)
    (spawn-warding-pair))
  (define-values (write-warden write-incanter)
    (spawn-warding-pair))
  (define-values (stop-warden stop-incanter)
    (spawn-warding-pair))

  (define inbox (spawn ^inbox read-warden write-warden stop-warden))
  (values (spawn-named '^inbox-reader ^inbox-op read-incanter inbox)
          (spawn-named '^inbox-writer ^inbox-op write-incanter inbox)
          (spawn-named '^inbox-stop! ^inbox-op stop-incanter inbox)))

(define inbox-env
  (persistence-env-compose
   (namespace-env (goblins actor-lib inbox) ^inbox ^inbox-op)
   queue-env
   ward-env))
