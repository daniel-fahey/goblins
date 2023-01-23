;;; Copyright 2022 David Thompson
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
  #:use-module (goblins vat))

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
    ;; TODO: Change #:name argument to incorporate the vat name once
    ;; that information is easy to obtain.
    (make-language #:name "goblins"
                   #:title "Goblins"
                   #:reader (language-reader scheme)
                   #:compilers (language-compilers scheme)
                   #:decompilers (language-decompilers scheme)
                   #:evaluator vat-eval
                   #:printer (language-printer scheme)
                   #:make-default-environment
                   (language-make-default-environment scheme)))
  goblins-language)

(define-meta-command ((enter-vat goblins) repl exp)
  "enter-vat vat
Enter a sub-REPL where all expressions are evaluated within VAT."
  (start-interpreted-repl
   (make-goblins-language (repl-eval repl exp))))
