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

(define-library (goblins utils error-handling)
  (export display-backtrace*
          capture-current-stack)
  (cond-expand
   (hoot
    (import (guile)
            ;; guard isn't used in core.scm, and it's used in
            ;; hoot error-handling) so it causes issues trying
            ;; to import it here. Therefore we skip it.
            (except (ice-9 exceptions) guard)
            (only (hoot exceptions)
                  exception-with-source?
                  exception-source-file
                  exception-source-line
                  exception-source-column)
            (hoot error-handling)))
   (guile
    (import (guile)
            (ice-9 exceptions))))

  (begin
    (define (display-backtrace* err stack)
      (cond-expand
       (guile
        ;; When displaying a backtrace in fibers, it's possible that
        ;; terminal-width in (system repl debug) will throw an error trying
        ;; to call (string->number #f) because the COLUMNS environment
        ;; variable isn't set.  We're not entirely sure why this happens,
        ;; but to work around it we set COLUMNS to Guile's own default of 72
        ;; if it hasn't been set already.
        (unless (getenv "COLUMNS")
          (setenv "COLUMNS" "72"))
        ;; Specify stack frame range explicitly, otherwise display-backtrace
        ;; will display additional frames in the current stack for some
        ;; reason!
        (display-backtrace stack (current-error-port) 0 (stack-length stack)))
       (hoot
        (let ((origin (and (exception-with-origin? err)
                           (exception-origin err))))
          (call-with-values
              (lambda ()
                (if (exception-with-source? err)
                    (values (exception-source-file err)
                            (exception-source-line err)
                            (exception-source-column err))
                    (values #f #f #f)))
            (lambda (file line column)
              (print-backtrace stack origin file line column (current-error-port)))))))
      (newline (current-error-port)))

    (define* (capture-current-stack . args)
      (cond-expand
       (guile (apply make-stack args))
       ;; TODO: Ideally we'd be trimming the stack to get rid
       ;; of some core hoot and goblins machinary.
       (hoot (capture-stack (stack-height)))))))
