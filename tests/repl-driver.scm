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

(define-module (tests repl-driver)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (srfi srfi-9)
  #:use-module (system repl common)
  #:use-module (system repl repl)
  #:export (make-repl-driver
            call-with-repl-driver
            repl-driver?
            repl-driver-start
            repl-driver-stop
            repl-driver-eval
            repl-driver-meta
            repl-driver-run
            repl-driver-get-output))

;; Going to use non-blocking I/O to drive the REPL.
(install-suspendable-ports!)

;; Generate a fresh user module with a unique name.
(define (fresh-user-module)
  (let ((module (resolve-module (list (gensym "repl-driver-")) #f)))
    (beautify-user-module! module)
    module))

(define-record-type <repl-driver>
  (%make-repl-driver input-port output-port
                     repl-input-port repl-output-port)
  repl-driver?
  (input-port repl-driver-input-port)
  (output-port repl-driver-output-port)
  (repl-input-port repl-driver-repl-input-port)
  (repl-output-port repl-driver-repl-output-port)
  (pid repl-driver-pid set-repl-driver-pid!)
  (continuation repl-driver-continuation
                set-repl-driver-continuation!))

(define (make-repl-driver)
  (match-let (((repl-in . driver-out) (pipe))
              ((driver-in . repl-out) (pipe)))
    ;; Don't block waiting for REPL output.
    (let ((flags (fcntl driver-in F_GETFL)))
      (fcntl driver-in F_SETFL (logior O_NONBLOCK flags)))
    (%make-repl-driver driver-in driver-out repl-in repl-out)))

(define repl-driver-prompt-tag (make-prompt-tag 'repl-driver))

(define* (repl-driver-start driver #:key (language (current-language)))
  (let ((driver-input-port (repl-driver-input-port driver))
        (driver-output-port (repl-driver-output-port driver))
        (repl-input-port (repl-driver-repl-input-port driver))
        (repl-output-port (repl-driver-repl-output-port driver)))
    (match (primitive-fork)
      (0 ; child
       ;; Close the driver sides of the pipes.
       (close-port driver-input-port)
       (close-port driver-output-port)
       (save-module-excursion
        (lambda ()
          (parameterize ((current-input-port repl-input-port)
                         (current-output-port repl-output-port)
                         (current-error-port repl-output-port))
            (set-current-module (fresh-user-module))
            (run-repl (make-repl language))
            ;; Terminate the process without unwinding the Scheme
            ;; stack.  Using regular 'exit' would allow exception
            ;; handlers to intercept the attempt to exit the program.
            (primitive-exit 0)))))
      (pid ; parent
       ;; Close the REPL sides of the pipes.
       (close-port repl-input-port)
       (close-port repl-output-port)
       (set-repl-driver-pid! driver pid)
       ;; Flush all the intro text that is printed when a REPL starts.
       (repl-driver-get-output driver)))))

(define (repl-driver-stop driver)
  (let ((pid (repl-driver-pid driver)))
    ;; Close the pipe, which will close the REPL.  The child process
    ;; will then exit.
    (close (repl-driver-output-port driver))
    (waitpid pid)))

(define (call-with-repl-driver proc)
  (let ((driver (make-repl-driver)))
    (dynamic-wind
      (lambda () (repl-driver-start driver))
      (lambda () (proc driver))
      (lambda () (repl-driver-stop driver)))))

(define (repl-driver-read-line driver)
  ;; Read a line of input.  If a full line can't be read, return #f.
  (call/ec
   (lambda (return)
     ;; By bailing out when read-line would block, we are throwing
     ;; away the REPL prompt.
     (parameterize ((current-read-waiter (lambda _ (return #f))))
       (read-line (repl-driver-input-port driver))))))

(define (repl-driver-read-lines driver)
  ;; Read as many lines as possible and return a list of them.
  (let ((line (repl-driver-read-line driver)))
    (if line
        (cons line (repl-driver-read-lines driver))
        '())))

(define (repl-driver-get-output driver)
  ;; Block until there is something to read, but timeout eventually so
  ;; we don't hang out forever.
  (let loop ((i 0))
    ;; Wait 1/1000th of a second between retries.  Allow 5000 retries
    ;; for a total wait of ~5 seconds.
    (if (< i 5000)
        (or (call/ec
             (lambda (return)
               (parameterize ((current-read-waiter (lambda _ (return #f))))
                 (peek-char (repl-driver-input-port driver))
                 #t)))
            (begin
              (usleep 1000)
              (loop (+ i 1))))
        (error "timeout waiting for REPL output")))
  ;; Read all available lines and concatenate them into a single
  ;; string.
  (string-join (repl-driver-read-lines driver) "\n" 'suffix))

(define (repl-driver-eval driver exp)
  (let ((port (repl-driver-output-port driver)))
    (write exp port)
    (newline port)
    (force-output port)
    (repl-driver-get-output driver)))

(define (repl-driver-meta driver exp)
  (match exp
    ((name args ...)
     (let ((port (repl-driver-output-port driver)))
       (format port ",~a " name)
       (for-each (lambda (arg)
                   (write arg port)
                   (display " " port))
                 args)
       (newline port)
       (force-output port)
       (repl-driver-get-output driver)))))

;; For convenience:
(define (repl-driver-run driver commands)
  (let loop ((commands commands)
             (last-output #f))
    (match commands
      (() last-output)
      ((('eval exp) . rest)
       (loop rest (repl-driver-eval driver exp)))
      ((('meta . exp) . rest)
       (loop rest (repl-driver-meta driver exp))))))
