
;;;; test-driver.scm - Guile test driver for Automake testsuite harness

(define script-version "2019-01-15.13") ;UTC

;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2019 Alex Sassmannshausen <alex@pompo.co>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Commentary:
;;;
;;; This script provides a Guile test driver using the SRFI-64 Scheme API for
;;; test suites.  SRFI-64 is distributed with Guile since version 2.0.9.
;;;
;;; This script is a lightly modified version of the orignal written by
;;; Matthieu Lirzin.  The changes make it suitable for use as part of the
;;; guile-hall infrastructure.
;;;
;;;; Code:

(use-modules (ice-9 getopt-long)
             (ice-9 pretty-print)
             (ice-9 match)
             (ice-9 string-fun)
             (sxml simple)
             (srfi srfi-26)
             (srfi srfi-64))

(define (show-help)
  (display "Usage:
                         test-driver --test-name=NAME --log-file=PATH --trs-file=PATH
                         [--expect-failure={yes|no}] [--color-tests={yes|no}]
                         [--enable-hard-errors={yes|no}] [--brief={yes|no}}] [--]
               TEST-SCRIPT [TEST-SCRIPT-ARGUMENTS]
The '--test-name', '--log-file' and '--trs-file' options are mandatory.
"))

(define %options
  '((test-name                  (value #t))
    (log-file                   (value #t))
    (trs-file                   (value #t))
    (color-tests                (value #t))
    (expect-failure             (value #t)) ;XXX: not implemented yet
    (enable-hard-errors         (value #t)) ;not implemented in SRFI-64
    (brief                      (value #t))
    (help    (single-char #\h) (value #f))
    (version (single-char #\V) (value #f))))

(define (option->boolean options key)
  "Return #t if the value associated with KEY in OPTIONS is 'yes'."
  (and=> (option-ref options key #f) (cut string=? <> "yes")))

(define* (test-display field value  #:optional (port (current-output-port))
                       #:key pretty?)
  "Display 'FIELD: VALUE\n' on PORT."
  (if pretty?
      (begin
        (format port "~A:~%" field)
        (pretty-print value port #:per-line-prefix "+ "))
      (format port "~A: ~S~%" field value)))

(define* (result->string symbol #:key colorize?)
  "Return SYMBOL as an upper case string.  Use colors when COLORIZE is #t."
  (let ((result (string-upcase (symbol->string symbol))))
    (if colorize?
        (string-append (case symbol
                         ((pass)       "[0;32m")  ;green
                         ((xfail)      "[1;32m")  ;light green
                         ((skip)       "[1;34m")  ;blue
                         ((fail xpass) "[0;31m")  ;red
                         ((error)      "[0;35m")) ;magenta
                       result
                       "[m")          ;no color
        result)))

(define* (test-runner-gnu test-name #:key color? brief? out-port trs-port xml-port)
  "Return an custom SRFI-64 test runner.  TEST-NAME is a string specifying the
file name of the current the test.  COLOR? specifies whether to use colors,
and BRIEF?, well, you know.  OUT-PORT and TRS-PORT must be output ports.  The
current output port is supposed to be redirected to a '.log' file."
  ;; TODO: Explain why we need this.
  (define test-results '())

  (define (generate-test-case-id src-file src-line name)
    "Creates a (hopefully) unique ID for the test.
This prefers named tests as it 'slugifies' the name along with the path to
create an ID, similar to the reverse DNS unique names. Not all tests are
named and these are more prone to changing over time as it instead uses
the source file plus the line number for test."
    (define name->slug
      (if name
          (string-filter
           (lambda (char)
             ;; Allow only a-z
             (or
              (eq? char #\-)
              (and (char>=? char #\a)
                   (char<=? char #\z))))
           (string-replace-substring
            (string-downcase name)
            " "
            "-"))
          #f))
    (string-append
     (string-replace-substring src-file file-name-separator-string ".")
     "."
     (if name->slug
         name->slug
         (number->string src-line))))

  (define (test-result->junit result)
    (match result
      ((name 'pass src-file src-line failure-reason src)
       `(testcase (@ (id ,(generate-test-case-id src-file src-line name))
                     (name ,(if name name "")))))
      ((name result-kind src-file src-line failure-reason src)
       `(testcase (@ (id ,(generate-test-case-id src-file src-line name))
                     (name ,(if name name "")))
         (failure (@ (message ,failure-reason)
                     (type "error"))
                  ,(format #f "ERROR: ~a\nLocation: ~a:~a\n~a"
                           failure-reason
                           src-file src-line
                           src))))))


  (define (test-runner-result->failure-reason test-result)
    (cond ((eq? (test-result 'result-kind) 'pass) #f)
          ((eq? (test-result 'result-kind) 'skip) #f)
          ((eq? (test-result 'result-kind) 'xfail) #f)
          ;; Now the actual failure cases we want reasons for:
          ((eq? (test-result 'result-kind) 'xpass)
           ;; Test was expected to fail but actually passed
           (format #f "Test passed but expected failure"))
          ;; The only test result value left is fail.
          ;; Firstly, the most common, a test-eq(ual) which
          ;; got a different value than expected.
          ((and (test-result 'expected-value)
                (not (equal? (test-result 'expected-value)
                             (test-result 'actual-value))))
           (format #f "Test expected to get '~a' but got '~a'"
                   (test-result 'expected-value)
                   (test-result 'actual-value)))
          ((test-result 'actual-error)
           (format #f "Got an error: ~a" (test-result 'actual-error)))
          (else
           ;; Not sure what went wrong but it failed.
           (format #f "Something went wrong!"))))

  (define (test-results->junit-format runner results)
    (define number-of-tests
      (+ (test-runner-fail-count runner)
         (test-runner-pass-count runner)
         (test-runner-xpass-count runner)))

    (define number-of-failures
      (+ (test-runner-fail-count runner)
         (test-runner-xpass-count runner)))

    `(testsuites (@ (id ,(strftime "%Y%m%d_%H%M%S" (gmtime (current-time))))
                    (name ,(strftime "Test Run %Y%m%d_%H%M%S)" (gmtime (current-time))))
                    (tests ,number-of-tests)
                    (failures ,number-of-failures))
      (testsuite (@ (id ,(generate-test-case-id "" "" test-name))
                    (name ,test-name)
                    (failures ,number-of-failures))
                 ,@(map test-result->junit test-results))))

  (define (test-on-test-begin-gnu runner)
    ;; Procedure called at the start of an individual test case, before the
    ;; test expression (and expected value) are evaluated.
    (let ((result (cute assq-ref (test-result-alist runner) <>)))
      (format #t "test-name: ~A~%" (result 'test-name))
      (format #t "location: ~A~%"
              (string-append (result 'source-file) ":"
                             (number->string (result 'source-line))))
      (test-display "source" (result 'source-form) #:pretty? #t)))

  (define (test-on-test-end-gnu runner)
    ;; Procedure called at the end of an individual test case, when the result
    ;; of the test is available.
    (let* ((results (test-result-alist runner))
           (result? (cut assq <> results))
           (result  (cut assq-ref results <>)))

      (set! test-results
            (cons
             (list (result 'test-name) (result 'result-kind)
                   (result 'source-file) (result 'source-line)
                   (test-runner-result->failure-reason result)
                   (result 'source-form))
             test-results))

      (unless brief?
        ;; Display the result of each test case on the console.
        (format out-port "~A: ~A - ~A~%"
                (result->string (test-result-kind runner) #:colorize? color?)
                test-name (test-runner-test-name runner)))
      (when (result? 'expected-value)
        (test-display "expected-value" (result 'expected-value)))
      (when (result? 'expected-error)
        (test-display "expected-error" (result 'expected-error) #:pretty? #t))
      (when (result? 'actual-value)
        (test-display "actual-value" (result 'actual-value)))
      (when (result? 'actual-error)
        (test-display "actual-error" (result 'actual-error) #:pretty? #t))
      (format #t "result: ~a~%" (result->string (result 'result-kind)))
      (newline)
      (format trs-port ":test-result: ~A ~A~%"
              (result->string (test-result-kind runner))
              (test-runner-test-name runner))))

  (define (test-on-group-end-gnu runner)
    ;; Procedure called by a 'test-end', including at the end of a test-group.
    (let ((fail (or (positive? (test-runner-fail-count runner))
                    (positive? (test-runner-xpass-count runner))))
          (skip (or (positive? (test-runner-skip-count runner))
                    (positive? (test-runner-xfail-count runner)))))
      ;; XXX: The global results need some refinements for XPASS.
      (format trs-port ":global-test-result: ~A~%"
              (if fail "FAIL" (if skip "SKIP" "PASS")))
      (format trs-port ":recheck: ~A~%"
              (if fail "yes" "no"))
      (format trs-port ":copy-in-global-log: ~A~%"
              (if (or fail skip) "yes" "no"))
      (when brief?
        ;; Display the global test group result on the console.
        (format out-port "~A: ~A~%"
                (result->string (if fail 'fail (if skip 'skip 'pass))
                                #:colorize? color?)
                test-name))

      (format xml-port "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n")
      (sxml->xml (test-results->junit-format runner test-results) xml-port)
      #f))

  (let ((runner (test-runner-null)))
    (test-runner-on-test-begin! runner test-on-test-begin-gnu)
    (test-runner-on-test-end! runner test-on-test-end-gnu)
    (test-runner-on-group-end! runner test-on-group-end-gnu)
    (test-runner-on-bad-end-name! runner test-on-bad-end-name-simple)
    runner))

;;;
;;; Entry point.
;;;

(define (log-file->junit-xml-file log-file)
  ;; I cannot seem to find a way to add other options to automake that allow for
  ;; adding other file outputs than ".log" and ".trs". Ideally we'd have
  ;; automake pass in another log file name rather than doing this.
  (string-append (string-drop-right log-file 3) "xml"))

(define (main . args)
  (let* ((opts   (getopt-long (command-line) %options))
         (option (cut option-ref opts <> <>)))
    (cond
     ((option 'help #f)    (show-help))
     ((option 'version #f) (format #t "test-driver.scm ~A" script-version))
     (else
      (let ((log (open-file (option 'log-file "") "w0"))
            (trs (open-file (option 'trs-file "") "wl"))
            (xml (open-file (log-file->junit-xml-file (option 'log-file "")) "wl"))
            (out (duplicate-port (current-output-port) "wl")))
        (redirect-port log (current-output-port))
        (redirect-port log (current-warning-port))
        (redirect-port log (current-error-port))
        (test-with-runner
            (test-runner-gnu (option 'test-name #f)
                             #:color? (option->boolean opts 'color-tests)
                             #:brief? (option->boolean opts 'brief)
                             #:out-port out #:trs-port trs #:xml-port xml)
          (load-from-path (option 'test-name #f)))
        (close-port log)
        (close-port trs)
        (close-port xml)
        (close-port out))))
    (exit 0)))
