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
(use-modules (sxml simple)
             (sxml xpath)
             (ice-9 match)
             (srfi srfi-1))

(define output-file
  (open-file (second (command-line)) "wl"))
(define junit-xml-files (cdr (cdr (command-line))))

;; First step is to parse each file
(define junit-xml
  (map (lambda (file-name)
         (xml->sxml (open-file file-name "r")))
       junit-xml-files))

;; Now calculate the total number of failures across all tests in all files
(define total-tests
  (apply
   +
   (map
    (lambda (doc)
      (string->number (cadr (assq 'tests ((sxpath '(testsuites @ (tests))) doc)))))
    junit-xml)))

(define total-failures
  (apply
   +
   (map
    (lambda (doc)
      (string->number (cadr (assq 'failures ((sxpath '(testsuites @ (failures))) doc)))))
    junit-xml)))

;; ;; Now work on outputting all of these in a single file.
(define combined-xml
  `(testsuites (@ (id ,(strftime "%Y%m%d_%H%M%S" (gmtime (current-time))))
                  (name ,(strftime "Test Run %Y%m%d_%H%M%S)" (gmtime (current-time))))
                  (tests ,total-tests)
                  (failures ,total-failures))
    ,@(map
       (lambda (sxml-document)
         ;; Skip by the *TOP* and testsuites sections to extract the testsuite property
         (third (third sxml-document)))
       junit-xml)))

(format output-file "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n")
(sxml->xml combined-xml output-file)
(close-port output-file)
