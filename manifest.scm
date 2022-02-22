(use-modules (guix packages)
             (gnu packages guile-xyz)
             (ice-9 match)
             (srfi srfi-1))

(define %here (dirname (current-filename)))

(packages->manifest
 (cons*
  emacs emacs-geiser ; For hacking on goblins inside the development environment
  guile-hall
  (filter-map
   (match-lambda
     ((_ (? package? package) output) (list package output))
     ((_ (? package? package)) package)
     (else #f))
   (package-development-inputs
    (load (string-append %here "/guix.scm"))))))
