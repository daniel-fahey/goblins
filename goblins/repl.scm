;;; Copyright 2022-2023 David Thompson
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

(define-module (goblins repl)
  #:use-module (system base compile)
  #:use-module (system base language)
  #:use-module (system repl common)
  #:use-module (system repl command)
  #:use-module (system repl debug)
  #:use-module (system repl repl)
  #:use-module (system vm loader)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9))

;; Special exception type that REPL commands will catch in order to
;; print out friendly error messages.
(define &goblins-repl-error
  (make-exception-type '&goblins-repl-error &error '()))

(define make-goblins-repl-error (record-constructor &goblins-repl-error))

(define (repl-error message)
  (raise-exception
   (make-exception (make-goblins-repl-error)
                   (make-exception-with-message message))))

(define (call-with-goblins-error-messages thunk)
  (with-exception-handler (lambda (e)
                            (display (exception-message e))
                            (newline))
    thunk
    #:unwind? #t
    #:unwind-for-type &goblins-repl-error))

(define-syntax-rule (with-goblins-error-messages body ...)
  (call-with-goblins-error-messages (lambda () body ...)))

;; This type stores a vat event trace (as a vector rather than a list)
;; and an index into that vector, for the purpose of moving up/down
;; the trace like we're used to with stack frames.
(define-record-type <vat-debug>
  (make-vat-debug trace index)
  vat-debug?
  (trace vat-debug-trace)
  (index vat-debug-index set-vat-debug-index!))

(define (vat-debug-max-index debug)
  (- (vector-length (vat-debug-trace debug)) 1))

(define (vat-debug-bottom? debug)
  (= (vat-debug-index debug) 0))

(define (vat-debug-top? debug)
  (= (vat-debug-index debug)
     (vat-debug-max-index debug)))

(define (vat-debug-current-event debug)
  (vector-ref (vat-debug-trace debug) (vat-debug-index debug)))

(define (vat-debug-up! debug)
  (set-vat-debug-index! debug
                        (min (+ (vat-debug-index debug) 1)
                             (vat-debug-max-index debug))))

(define (vat-debug-down! debug)
  (set-vat-debug-index! debug (max (- (vat-debug-index debug) 1) 0)))

(define current-vat-debug (make-parameter #f))

(define (current-vat-debug*)
  (let ((debug (current-vat-debug)))
    (if (vat-debug? debug)
        debug
        (repl-error "Not currently debugging a vat error."))))

;; This code is based on error-string in (system repl
;; exception-handling) and adapted to work with Guile's new exception
;; objects.
(define (error-message stack e)
  (let ((key (exception-kind e))
        (args (exception-args e)))
    (call-with-output-string
      (lambda (port)
        (let ((frame (and (< 0 (vector-length stack)) (vector-ref stack 0))))
          (print-exception port frame key args))))))

;; Our Goblins REPLs *have to be* interpreted otherwise our special
;; eval procedure won't be used and that's what makes the whole thing
;; work.
(define* (start-interpreted-repl language #:key debug)
  (let ((repl (make-repl language debug)))
    (repl-option-set! repl 'interp #t)
    (run-repl repl)))

(define (enter-debugger language e)
  (let* ((stack (narrow-stack->vector (actormap-turn-error-stack e) 0))
         (msg (error-message stack e))
         (event (vat-turn-error-event e))
         (trace (list->vector (vat-event-trace event)))
         (debug (make-debug stack 0 msg)))
    (parameterize ((current-vat-debug (make-vat-debug trace 0)))
      ;; Mimicking Guile's debugger welcome message because starting a
      ;; debug REPL doesn't do it!
      (format #t "~a\n" msg)
      (format #t "Entering a new prompt. ")
      (format #t "Type `,bt' for a backtrace or `,q' to continue.\n")
      (start-interpreted-repl language #:debug debug)
      ;; The previous procedure returns the empty list, which would get
      ;; printed as a return value when the sub-repl is exited.  That's
      ;; a bit weird, so force the return value to be unspecified
      ;; instead.
      *unspecified*)))

(define (call-with-goblins-debugger language thunk)
  (with-exception-handler (lambda (e) (enter-debugger language e))
    thunk
    #:unwind? #t
    #:unwind-for-type &vat-turn-error))

;; We make a language object per-vat so that we can evaluate
;; expressions in the context of a specific vat without having to
;; introduce dynamic scoping via parameters.
(define* (make-goblins-language vat)
  (define scheme (lookup-language 'scheme))
  (define (vat-eval exp env)
    ;; Compile the expression to bytecode and load it into a thunk
    ;; that we can pass to the vat.
    (define compiled-thunk
      (load-thunk-from-memory
       (compile exp #:to 'bytecode #:env env)))
    (call-with-goblins-debugger
     goblins-language
     (lambda ()
       (call-with-vat vat compiled-thunk))))
  ;; The Goblins language is just Scheme with a special evaluator that
  ;; does vat magic.
  (define goblins-language
    (make-language #:name (format #f "goblins/~a"
                                  (or (vat-name vat) (vat-id vat)))
                   #:title "Goblins"
                   #:reader (language-reader scheme)
                   #:compilers (language-compilers scheme)
                   #:decompilers (language-decompilers scheme)
                   #:evaluator vat-eval
                   #:printer (language-printer scheme)
                   #:make-default-environment
                   (language-make-default-environment scheme)))
  goblins-language)

(define current-vat (make-parameter #f))

(define (current-vat*)
  (let ((vat (current-vat)))
    (if (vat? vat)
        vat
        (repl-error "Not in a vat.  Use ,enter-vat first."))))

(define-meta-command ((vats goblins) repl)
  "vats
Display a list of vats."
  (match (sort (all-vats)
               (lambda (a b)
                 (< (vat-id a) (vat-id b))))
    (()
     (format #t "No vats.\n"))
    (vats
     (format #t "id\tstatus\tlogging\tname\n")
     (format #t "--\t------\t-------\t----\n")
     (for-each (lambda (vat)
                 (let ((id (vat-id vat))
                       (name (or (vat-name vat) ""))
                       (status (if (vat-running? vat) "running" "stopped"))
                       (logging (if (vat-logging? vat) "enabled" "disabled")))
                   (format #t "~a\t~a\t~a\t~a\n"
                           id status logging name)))
               vats))))

(define (maybe-lookup-vat x)
  (if (vat? x) x (lookup-vat x)))

(define-meta-command ((enter-vat goblins) repl exp)
  "enter-vat vat
Enter a sub-REPL where all expressions are evaluated within VAT."
  (with-goblins-error-messages
   (let ((vat (maybe-lookup-vat (repl-eval repl exp))))
     (if (vat? vat)
         (parameterize ((current-vat vat))
           (format #t "Entering vat '~a'.  Type ',q' to exit.  Type ',help goblins' for help.\n"
                   (or (vat-name vat) (vat-id vat)))
           (start-interpreted-repl
            (make-goblins-language vat)))
         (repl-error (format #f "Not a vat: ~s" vat))))))

(define-meta-command ((vat-log-enable goblins) repl)
  "vat-log-enable
Enable vat event logging for the current vat."
  (with-goblins-error-messages
   (set-vat-logging! (current-vat*) #t)
   (display "Logging enabled.\n")))

(define-meta-command ((vat-log-disable goblins) repl)
  "vat-log-disable
Disable vat event logging for the current vat."
  (with-goblins-error-messages
   (set-vat-logging! (current-vat*) #f)
   (display "Logging disabled.\n")))

(define (check-logging-status vat)
  (unless (vat-logging? vat)
    (display "warning: Logging is disabled.  Use ,vat-log-enable to begin logging.\n")))

;; Symbolic representation of a vat event for the purpose of printing.
(define (vat-event->list event)
  (let ((msg (vat-event-message event)))
    (cons (if (vat-send-event? event)
              'send
              'receive)
          (cond
           ((listen-request? msg)
            `(listen ,(message-or-request-to msg)))
           ((questioned? msg)
            `(question ,(message-or-request-to msg)))
           (else
            (cons (message-to msg) (message-args msg)))))))

(define (symbolic-event event)
  (let ((msg (vat-event-message event)))
    (cond
     ((message? msg)
      `(message ,(message-to msg) ,@(message-args msg)))
     ((listen-request? msg)
      `(listen ,(message-or-request-to msg)))
     ((questioned? msg)
      `(question ,(message-or-request-to msg)))
     (else
      (repl-error (format #f "unknown message: ~a" msg))))))

(define-meta-command ((vat-tail goblins) repl #:optional (n 10))
  "vat-tail [N]
Display the most recent N messages in the current vat."
  (with-goblins-error-messages
   (let* ((vat (current-vat*))
          (len (vat-log-length vat)))
     (check-logging-status vat)
     (let loop ((i (max (- len n) 0))
                (prev-churn #f))
       (unless (= i len)
         (let* ((event (vat-log-ref vat i))
                (churn (vat-event-churn event)))
           (unless (eq? churn prev-churn)
             (format #t "Churn ~a:\n" churn))
           (format #t "  ~a: ~s\n"
                   (vat-event-timestamp event)
                   (vat-event->list event))
           (loop (+ i 1) churn)))))))

(define (vat-log-ref-by-time* vat timestamp)
  (check-logging-status vat)
  (let ((event (vat-log-ref-by-time vat timestamp)))
    (if (vat-event? event)
        event
        (repl-error
         (format #f "No event for logical timestamp ~a." timestamp)))))

(define-meta-command ((vat-trace goblins) repl #:optional timestamp)
  "vat-trace [TIMESTAMP]
Display a backtrace of events starting from TIMESTAMP in the current vat."
  (define (print-churn-id event)
    (format #t "  Churn ~a:\n" (vat-event-churn event)))
  (define (print-event event prev-event)
    (let ((vat-connector (vat-event-connector event))
          (prev-vat-connector (and prev-event (vat-event-connector prev-event)))
          (prev-churn (and prev-event (vat-event-churn prev-event))))
      (cond
       ((not (eq? vat-connector prev-vat-connector))
        (format #t "In vat ~a:\n" (vat-connector 'name))
        (print-churn-id event))
       ((not (= (vat-event-churn event) prev-churn))
        (print-churn-id event)))
      (format #t "    ~a: ~s\n"
              (vat-event-timestamp event)
              (symbolic-event event))))
  (with-goblins-error-messages
   (let* ((vat (current-vat*))
          (debug (current-vat-debug))
          (trace (cond
                  ;; User provided a timestamp.
                  ((number? timestamp)
                   (vat-event-trace
                    (vat-log-ref-by-time* vat timestamp)))
                  ;; No timestamp provided, but we are in a debugger,
                  ;; so use the current debugging trace narrowed to
                  ;; the current debug index.
                  (debug
                   (drop (vector->list (vat-debug-trace debug))
                         (vat-debug-index debug)))
                  ;; No timestamp provided and we are not in a
                  ;; debugger, use the current vat timestamp.
                  (else
                   (vat-event-trace
                    (vat-log-ref-by-time* vat (vat-clock vat)))))))
     (let loop ((events (reverse trace))
                (prev-event #f))
       (match events
         (() *unspecified*)
         ;; Collapse cross-vat send+receive events into a single frame
         ;; of the trace.  The send event gets rendered but not the
         ;; redundant receive event.
         (((? vat-send-event? event) _ . rest)
          (print-event event prev-event)
          (loop rest event))
         ((event . rest)
          (print-event event prev-event)
          (loop rest event)))))))

(define-meta-command ((vat-tree goblins) repl #:optional timestamp)
  "vat-tree [TIMESTAMP]
Display a tree view of events starting at TIMESTAMP in the current vat."
  (define (print-branches levels)
    (match levels
      (() #t)
      ((branch?)
       (display (if branch? "├▸ " "└▸ ")))
      ((branch? . rest)
       (display (if branch? "│  " "   "))
       (print-branches rest))))
  (define (print-event event levels)
    (let* ((type (vat-event-type event))
           (msg (vat-event-message event))
           (to (message-or-request-to msg))
           (vat-connector (vat-event-connector event)))
      (print-branches levels)
      (format #t "Vat ~a, ~a: ~s\n"
              (vat-connector 'name)
              (vat-event-timestamp event)
              (symbolic-event event))))
  (define (print-list events levels)
    (match events
      ((event)
       (print-tree event (append levels (list #f))))
      ((event . rest)
       (print-tree event (append levels (list #t)))
       (print-list rest levels))))
  (define (print-tree tree levels)
    (match tree
      ;; Collapse cross-vat send+receive events into a single level of
      ;; the tree.  The send event gets rendered but not the redundant
      ;; receive event.
      (((? vat-send-event? send-event) (_ children ...))
       (print-event send-event levels)
       (print-list children levels))
      ((event children ..1)
       (print-event event levels)
       (print-list children levels))
      (event
       (print-event event levels))))
  (with-goblins-error-messages
   (let* ((vat (current-vat*))
          (event (vat-log-ref-by-time* vat (or timestamp (vat-clock vat)))))
     (print-tree (vat-event-tree event) '()))))

(define-meta-command ((vat-errors goblins) repl)
  "vat-errors
Display a list of errors that have occurred in the current vat."
  (define (print-error event exception)
    (let* ((stack (actormap-turn-error-stack exception))
           (frame (stack-ref stack 0)))
      (format #t "At churn ~a, event ~a, file ~a:\n  In procedure ~a: ~a\n"
              (vat-event-churn event)
              (vat-event-timestamp event)
              (match (frame-source frame)
                ((_ file-name line . column)
                 (format #f "~a:~a:~a"
                         (if file-name
                             (basename file-name)
                             "unknown")
                         line column))
                (_ "unknown"))
              (or (frame-procedure-name frame)
                  "unknown")
              (if (and (exception-with-message? exception)
                       (exception-with-irritants? exception))
                  (apply format #f (exception-message exception)
                         (exception-irritants exception))
                  ""))))
  (with-goblins-error-messages
   (let ((vat (current-vat*)))
     ;; Sort errors by timestamp.
     (match (sort (vat-log-errors vat)
                  (match-lambda*
                    (((a . _) (b . _))
                     (< (vat-event-timestamp a)
                        (vat-event-timestamp b)))))
       (()
        (format #t "No errors.\n"))
       (errors
        (for-each (match-lambda
                    ((event . e)
                     (print-error event e)))
                  errors))))))

(define-meta-command ((vat-debug goblins) repl timestamp)
  "vat-debug [TIMESTAMP]
Debug error associated with the event at TIMESTAMP."
  (with-goblins-error-messages
   (let* ((vat (current-vat*))
          (timestamp* (if (integer? timestamp)
                          timestamp
                          (repl-eval repl timestamp)))
          (event (vat-log-ref-by-time* vat timestamp*))
          (exception (vat-log-error-for-event vat event)))
     (if exception
         (enter-debugger (repl-language repl) exception)
         (format #t "No error at event ~a" timestamp*)))))

(define (print-current-vat-debug-event debug)
  (let ((event (vat-debug-current-event debug)))
    (format #t "Vat ~a, event ~a: ~s\n"
            ((vat-event-connector event) 'name)
            (vat-event-timestamp event)
            (vat-event->list event))))

(define-meta-command ((vat-up goblins) repl)
  "vat-up
Move to the previous event in the current vat debug trace."
  (with-goblins-error-messages
   (let ((debug (current-vat-debug*)))
     (if (vat-debug-top? debug)
         (format #t "Already at oldest event.\n")
         (begin
           (vat-debug-up! debug)
           (print-current-vat-debug-event debug))))))

(define-meta-command ((vat-down goblins) repl)
  "vat-down
Move to the next event in the current vat debug trace."
  (with-goblins-error-messages
   (let ((debug (current-vat-debug*)))
     (if (vat-debug-bottom? debug)
         (format #t "Already at most recent event.\n")
         (begin
           (vat-debug-down! debug)
           (print-current-vat-debug-event debug))))))

(define-meta-command ((vat-peek goblins) repl refr . args)
  "vat-peek REFR [ARGS ...]
Send ARGS to REFR using the snapshot for the current debugger event."
  (with-goblins-error-messages
   (let ((debug (current-vat-debug*)))
     (let ((event (vat-debug-current-event debug)))
       (format #t "~s\n"
               (apply actormap-peek (vat-event-snapshot event)
                      (repl-eval repl `(list ,refr ,@args))))))))
