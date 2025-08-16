;;; Copyright 2024 Christine Lemmer-Webber
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

(define-module (goblins actor-lib io)
  #:use-module (ice-9 match)
  #:use-module (goblins core)
  #:use-module (goblins default-vat-scheduler)
  #:use-module (goblins vat)
  #:use-module (goblins inbox)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins utils error-handling)
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (fibers scheduler)
  #:export (^io ^read-write-io))

;; Not exported for now, but maybe someday it would be useful to export?
(define* (run-wrapped wrapped #:key init cleanup)
  "Set up a fiber which processes commands for WRAPPED, a wrapped resource

In general, this is more low level than most users will want,
and using ^io is generally preferred.

Returns two values to its continuation:
 - RUN-PROC: a procedure which accepts two arguments, a procedure to run
   and a resolver to resolve a promise with the answer
 - STOP!: a thunk which halts the fiber"
  (define-values (enq-ch deq-ch stop-inbox?)
    (spawn-delivery-agent #:scheduler (current-vat-scheduler)))
  (define (run-proc proc fulfill-me)
    (put-message enq-ch (cons proc fulfill-me)))
  (define (stop!)
    (put-message enq-ch 'halt))

  (syscaller-free-fiber
   (lambda ()
     ;; Run init function, if appropriate
     (when init (init wrapped))
     ;; Main loop
     (let lp ()
       ;; Whether we loop will be determined by whether we run
       ;; a procedure or break, determined by unpacking the argument
       ;; from the deq-ch
       (define loop?
         (perform-operation
          (wrap-operation
           (get-operation deq-ch)
           (match-lambda
             ;; Got a message from `run-proc' above
             ((proc . resolver)
              ;; This is the default behavior we'll run
              (define (run-and-send)
                ;; run the procedure with wrapped resource
                (define return-val
                  (proc wrapped))
                ;; resolve promise w/ answer
                (<-np-extern resolver 'fulfill return-val))
              ;; In case something goes wrong
              (define (handle-exn exn)
                ;; Print exception
                (define stack
                  (capture-current-stack #t handle-exn))
                (format (current-error-port)
                        "Error in IO handling wrapped resource ~a:\n"
                        wrapped)
                (format (current-error-port) "~a\n" exn)
                (display-backtrace* exn stack)
                ;; Break promise
                (<-np-extern resolver 'break exn))
              (with-exception-handler handle-exn
                run-and-send)
              #t)       ; request loop!
             ;; got a message from `stop!' above
             ('halt
              (signal-condition! stop-inbox?)
              (when cleanup
                (cleanup wrapped))      ; run cleanup, if appropriate
              #f)))))                   ; please don't loop!
       ;; loop if appropriate
       (when loop? (lp)))))
  (values run-proc stop!))

;; TODO: We need to set up a guardian to automatically clean up when
;; this actor reference goes out of scope
(define* (^io bcom wrapped
              #:key init cleanup)
  "Spawn an interface for running commands over WRAPPED

`^io' sets up a separate fiber which processes one command at a
time (taking the wrapped resource as their only argument) if this
actor is sent a procedure, or halts if given the 'halt symbol.  If
INIT is provided as a procedure, it is run first before any other
commands are processed.  When the fiber halts, CLEANUP is run."
  (define-values (run-me stop-me!)
    (run-wrapped wrapped #:init init #:cleanup cleanup))
  (define main-beh
    (match-lambda
      ((? procedure? proc)
       (define-values (io-vow io-resolver)
         (spawn-promise-and-resolver))
       (run-me proc io-resolver)
       io-vow)
      ('halt (stop-me!)
             (bcom halted-beh))
      (something-else
       (error "^io expects a procedure or the 'halt symbol, got:"
              something-else))))
  (define halted-beh
    (lambda _ (error "IO access halted!")))
  main-beh)

(define* (^read-write-io bcom wrapped #:key init cleanup)
  "Spawn a read and write interface for running commands over WRAPPED

Like the ^io object, this spawns a seperate fiber which processes one
command at a time. This however allows for both reading and writing by
having a 'read method (taking in a procedure as its only argument),
and a 'write method (also taking a single procedure as its only
argument). You can read and write at the same time to the same WRAPPED
resource without blocking the other.

Like ^io, this supports a 'halt method which stops both fibers and
runs any CLEANUP provided."
  (define init-completed?
    (make-condition))
  (define read-io
    (spawn ^io wrapped
           #:init (lambda (inner-wrapped)
                    (when init
                      (init inner-wrapped))
                    (signal-condition! init-completed?))
           #:cleanup cleanup))
  (define write-io
    (spawn ^io wrapped
           #:init
           (lambda (_inner-wrapped)
             (wait init-completed?))))

  (define halted-beh
    (lambda _ (error "IO access halted!")))

  (methods
   [(write proc) ($ write-io proc)]
   [(read proc) ($ read-io proc)]
   [(halt)
    ($ read-io 'halt)
    ($ write-io 'halt)
    (bcom halted-beh)]))
