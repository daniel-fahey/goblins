(use-modules (sxml simple)
             (sxml xpath)
             (ice-9 format)
             (ice-9 match)
             (ice-9 threads)
             (ice-9 popen)
             (srfi srfi-1)
             (srfi srfi-64))

(define junit-output-filename "test-results.xml")

(define (replace-file-extension filename new-extension)
  (let ((extension-index (string-rindex filename #\.)))
    (if extension-index
        (string-append (substring filename 0 extension-index)
                       new-extension)
        (error "No file extension found"))))

(define (combine-junit-xml-files junit-xml-files)
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

  ;; Now work on outputting all of these in a single file.
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

(match (command-line)
  [(_ tests ...)
   (par-for-each
    (lambda (test-name)
      (let ((log-filename (replace-file-extension test-name ".log"))
            (trs-filename (replace-file-extension test-name ".trs")))
        (system* "guile" "-e" "main"
                 "build-aux/test-driver.scm"
                 test-name
                 (string-append "--test-name=" test-name)
                 (string-append "--log-file=" log-filename)
                 (string-append "--trs-file=" trs-filename)
                 "--"
                 "guile"
                 "-L" "."
                 test-name)))
    tests)

   ;; Now build the combined junit file.
   (let* ((xml-files (map (lambda (filename) (replace-file-extension filename ".xml")) tests))
          (combined-sxml (combine-junit-xml-files xml-files)))
     (call-with-output-file junit-output-filename
       (lambda (port)
         (format port "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n")
         (sxml->xml combined-sxml port))))]
  [something-else (error "Unknown usage" something-else)])
