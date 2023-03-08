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
  #:use-module (srfi srfi-9))

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
         (debug (make-debug stack 0 msg)))
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
    *unspecified*))

(define (call-with-goblins-debugger language thunk)
  (with-exception-handler (lambda (e) (enter-debugger language e))
    thunk
    #:unwind? #t
    #:unwind-for-type &actormap-turn-error))

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

(define-syntax-rule (when-in-vat body ...)
  (if (vat? (current-vat))
      (begin body ...)
      (format #t "Not in a vat.  Use ,enter-vat first.\n")))

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
  (let ((vat (maybe-lookup-vat (repl-eval repl exp))))
    (if (vat? vat)
        (parameterize ((current-vat vat))
          (start-interpreted-repl
           (make-goblins-language vat)))
        (format #t "Not a vat: ~s" vat))))

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

(define-meta-command ((vat-tail goblins) repl #:optional (n 10))
  "vat-tail [N]
Display the most recent N messages in the current vat."
  (when-in-vat
   (let* ((vat (current-vat))
          (len (vat-log-length vat)))
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
              (vat-event->list event))))
  (when-in-vat
   (let* ((vat (current-vat))
          (timestamp (or timestamp (vat-clock vat)))
          (event (vat-log-ref-by-time vat timestamp)))
     (let loop ((events (reverse (vat-event-trace event)))
                (prev-event #f))
       (match events
         (() *unspecified*)
         ((event . rest)
          (print-event event prev-event)
          (loop rest event)))))))

(define-meta-command ((vat-tree goblins) repl #:optional timestamp)
  "vat-tree [TIMESTAMP]
Display a tree view of events starting at TIMESTAMP in the current vat."
  (define (print-event event depth last?)
    (let* ((type (vat-event-type event))
           (msg (vat-event-message event))
           (to (message-or-request-to msg))
           (vat-connector (vat-event-connector event))
           (whitespace-depth (- depth 1))
           (whitespace (if (> whitespace-depth 0)
                           (make-string (* whitespace-depth 4) #\space)
                           ""))
           (indent (if (> depth 0)
                       (string-append whitespace
                                      (if last? "└" "├")
                                      "─► ")
                       "")))
      (format #t "~aVat ~a, ~a: ~s\n"
              indent
              (vat-connector 'name)
              (vat-event-timestamp event)
              (vat-event->list event))))
  (when-in-vat
   (let* ((vat (current-vat))
          (event (vat-log-ref-by-time vat (or timestamp (vat-clock vat)))))
     (let loop ((nodes (vat-event-tree event))
                (depth 0))
       (match nodes
         (() #t)
         (((event (children ...)) . rest)
          (print-event event depth (null? rest))
          (loop children (+ depth 1))
          (loop rest depth))
         ((event . rest)
          (print-event event depth (null? rest))
          (loop rest depth)))))))

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
  (when-in-vat
   (let ((vat (current-vat)))
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
  (when-in-vat
   (let* ((vat (current-vat))
          (event (vat-log-ref-by-time vat timestamp))
          (exception (vat-log-error-for-event vat event)))
     (if exception
         (enter-debugger (repl-language repl) exception)
         (format #t "No error at event ~a" timestamp)))))

(define-meta-command ((vat-peek-past goblins) repl timestamp refr . args)
  "vat-peek-past TIMESTAMP REFR [ARGS ...]
Send ARGS to REFR using historical actormap state at TIMESTAMP."
  (when-in-vat
   (let* ((vat (current-vat))
          (event (vat-log-ref-by-time vat timestamp)))
     (if event
         (format #t "~s\n"
                 (apply actormap-peek (vat-event-snapshot event)
                        (repl-eval repl `(list ,refr ,@args))))
         (format #t "no vat event with timestamp ~a\n" timestamp)))))
