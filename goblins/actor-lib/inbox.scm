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
  #:use-module  (goblins actor-lib queue)
  #:use-module (srfi srfi-11)
  #:export (spawn-inbox inbox-env))

(define-actor (^channel bcom read-warden write-warden stop-warden
                        #:optional
                        [messages (spawn ^queue)]
                        [pending (spawn ^queue)]
                        [stopped? #f])
  #:self-portrait (lambda () (list read-warden write-warden stop-warden messages stopped?))
  #:restore (lambda (read-warden write-warden stop-warden messages stopped?)
              (spawn ^channel read-warden write-warden stop-warden messages (spawn ^queue)
                     stopped?))

  (define (read-beh)
    (if ($ messages 'empty?)
        ;; We have no messages to give, give a promise to a message
        (let-values (((vow resolver) (spawn-promise-values)))
          ($ pending 'enqueue resolver)
          vow)
        ($ messages 'dequeue)))

  (define (write-beh message)
    (if ($ pending 'empty?)
        ($ messages 'enqueue message)
        (let ((waiting-resolver ($ pending 'dequeue)))
          ($ waiting-resolver 'fulfill message))))

  (define (defunct . _args)
    (error "No longer in use"))

  (define (stop-beh)
    (while (not ($ pending 'empty?))
      (let ((waiting-resolver ($ pending 'dequeue)))
        ($ waiting-resolver 'break 'inbox-closed)))
    (bcom (^channel bcom read-warden write-warden stop-warden #f #f #t)))

  (if stopped?
      defunct
      (let* ((warded-stop (ward stop-warden stop-beh))
             (warded-write-stop (ward write-warden write-beh #:extends warded-stop))
             (warded-write-stop-read (ward read-warden read-beh #:extends warded-write-stop)))
        warded-write-stop-read)))

(define-actor (^channel-op _bcom incanter channel)
  (lambda args
    (apply $ incanter channel args)))

(define (spawn-inbox)
  (define-values (read-warden read-incanter)
    (spawn-warding-pair))
  (define-values (write-warden write-incanter)
    (spawn-warding-pair))
  (define-values (stop-warden stop-incanter)
    (spawn-warding-pair))

  (define channel (spawn ^channel read-warden write-warden stop-warden))
  (values (spawn-named '^channel-reader ^channel-op read-incanter channel)
          (spawn-named '^channel-writer ^channel-op write-incanter channel)
          (spawn-named '^channel-stop! ^channel-op stop-incanter channel)))

(define inbox-env
  (make-persistence-env
   `((((goblins actor-lib inbox) ^channel) ,^channel)
     (((goblins actor-lib inbox) ^channel-op) ,^channel-op))
   #:extends (list queue-env ward-env)))
