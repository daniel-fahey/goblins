;;; Copyright 2022 Jessica Tallon
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

(use-modules (system vm coverage)
             (system vm vm)
	     (srfi srfi-11)
	     (srfi srfi-64)
	     (ice-9 ftw)
	     (ice-9 match))

;; Backported from guile (modules/system/vm/coverage.scm), this code
;; is LGPL v3. This is backported to get the modules keyword which
;; provided in bug#54911.
(define* (backport/coverage-data->lcov data port #:key (modules #f))
  "Traverse code coverage information DATA, as obtained with
`with-code-coverage', and write coverage information in the LCOV format to PORT.
The report will include all the modules loaded at the time coverage data was
gathered, even if their code was not executed."

  ;; Output per-file coverage data.
  (format port "TN:~%")
  (define source-files
    (filter
     (lambda (file)
       (or (not modules) (member file modules)))
     (instrumented-source-files data)))

  (for-each (lambda (file)
              (let ((path (search-path %load-path file)))
                (if (string? path)
                    (begin
                      (format port "SF:~A~%" path)
                      #;
                      (for-each dump-function procs)
                      (for-each (lambda (line+count)
                                  (let ((line  (car line+count))
                                        (count (cdr line+count)))
                                    (format port "DA:~A,~A~%"
                                            (+ 1 line) count)))
                                (line-execution-counts data file))
                      (let-values (((instr exec)
                                    (instrumented/executed-lines data file)))
                        (format port "LH: ~A~%" exec)
                        (format port "LF: ~A~%" instr))
                      (format port "end_of_record~%"))
                    (begin
                      (format (current-error-port)
                              "skipping source file: ~a~%"
                              file)))))
            source-files))


(define (get-scm-files dir)
  "Recursively finds all the .scm files in a given directory"
  (define (find-files path found)
    (match found
      ((name stat)
       (when (string=? (substring name (- (string-length name) 4)) ".scm")
	 (string-append path "/" name)))
      ((name stat children ...)
       (let ((new-path (if (string-null? path) name (string-append path "/" name))))
	 (map
	  (lambda (child) (find-files new-path child))
	  children)))))
  (define (flatten obj)
    (if (null? obj)
	(list)
	(if (list? (car obj))
	    (append (flatten (car obj)) (flatten (cdr obj)))
	    (cons (car obj) (flatten (cdr obj))))))
  (filter
   string?
   (flatten
    (find-files "" (file-system-tree dir)))))

(define (run-coverage)
  "Run coverage check and write the result out into the lcov.info file"
  (define goblins-modules
    (get-scm-files "goblins"))

  (define-values (data result)
    (with-code-coverage
     (lambda _
       (test-with-runner
	   (test-runner-simple)
	 (map load-from-path (get-scm-files "tests")))
       ;; This loads all goblins modules to get the test coverage
       ;; statistics on each, without it just shows tested modules.
       (map load-from-path goblins-modules))))

  (let ((port (open-output-file "lcov.info")))
    (backport/coverage-data->lcov data port #:modules goblins-modules)))
(run-coverage)
