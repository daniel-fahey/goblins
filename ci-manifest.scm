;; This file is waived into the public domain, as-is, no warranty provided.
;;
;; If the public domain doesn't exist where you live, consider
;; this a license which waives all copyright and neighboring intellectual
;; restrictions laws mechanisms, to the fullest extent possible by law,
;; as-is, no warranty provided.
;;
;; No attribution is required and you are free to copy-paste and munge
;; into your own project.

(use-modules (gnu packages code)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages graphviz)
             (gnu packages guile-xyz)
             (gnu packages tor)
             (gnu packages python)
             (guix packages)
             (ice-9 match)
             (srfi srfi-1))

(define %here (dirname (current-filename)))

(packages->manifest
 (cons*
  ;; Used for code coverage
  python
  lcov-cobertura ;; python-lcov-cobertura

  graphviz
  guile-lib
  guile-syntax-highlight
  lcov
  tor
  (filter-map
   (match-lambda
     ((_ (? package? package) output) (list package output))
     ((_ (? package? package)) package)
     (else #f))
   (package-development-inputs
    (load (string-append %here "/guix.scm"))))))
