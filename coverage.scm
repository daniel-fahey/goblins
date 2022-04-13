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
	     (srfi srfi-64)
	     (ice-9 ftw)
	     (ice-9 match))

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
  (define-values (data result)
    (with-code-coverage
     (lambda _
       (test-with-runner
	   (test-runner-simple)
	 (map load-from-path (get-scm-files "tests"))))))

  (let ((port (open-output-file "lcov.info")))
    (coverage-data->lcov data port)))
(run-coverage)
