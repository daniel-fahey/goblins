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
             (ice-9 format)
             (srfi srfi-1))

(define-values (output-file-name junit-xml-files)
  (match (command-line)
    ((_program-name output-file-name junit-xml-files ...)
     (values output-file-name junit-xml-files))))

;; First step is to parse each file
(define junit-xml
  (map (lambda (file-name)
         (call-with-input-file file-name xml->sxml))
       junit-xml-files))

;; Now calculate the total number of failures across all tests in all files
(define total-tests
  (reduce
   +
   0
   (map
    (lambda (doc)
      (match ((sxpath '(testsuites @ (tests))) doc)
        ((('tests total-tests))
         (string->number total-tests))))
    junit-xml)))

(define total-failures
  (reduce
   +
   0
   (map
    (lambda (doc)
      (match ((sxpath '(testsuites @ (failures))) doc)
        ((('failures total-failures))
         (string->number total-failures))))
    junit-xml)))

(define total-time
  (reduce
   +
   0
   (map
    (lambda (doc)
      (match ((sxpath '(testsuites @ (time))) doc)
        ((('time runtime))
         (string->number runtime))))
    junit-xml)))

;; ;; Now work on outputting all of these in a single file.
(define combined-xml
  `(testsuites (@ (id ,(strftime "%Y%m%d_%H%M%S" (gmtime (current-time))))
                  (name ,(strftime "Test Run %Y%m%d_%H%M%S)" (gmtime (current-time))))
                  (tests ,total-tests)
                  (failures ,total-failures)
                  (time ,(format #f "~f" total-time)))
    ,@(map
       (lambda (sxml-document)
         ;; Skip by the *TOP* and testsuites sections to extract the testsuite property
         (match sxml-document
           ((*TOP* xml-head (testsuites _p1 testsuite ...)) testsuite)))
       junit-xml)))

(call-with-output-file output-file-name
  (lambda (port)
    (format port "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n")
    (sxml->xml combined-xml port)))
