;; This file is waived into the public domain, as-is, no warranty provided.
;;
;; If the public domain doesn't exist where you live, consider
;; this a license which waives all copyright and neighboring intellectual
;; restrictions laws mechanisms, to the fullest extent possible by law,
;; as-is, no warranty provided.
;;
;; No attribution is required and you are free to copy-paste and munge
;; into your own project.

(use-modules
  (guix gexp)
  (guix packages)
  ((guix licenses) #:prefix license:)
  (guix download)
  (guix git-download)
  (guix build-system gnu)
  (gnu packages)
  (gnu packages autotools)
  (gnu packages gnupg)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo)
  (gnu packages tls)
  (srfi srfi-1))

(define guile-websocket-next
  (let ((commit "6adfc6605b39072c3631f9d080b1a1da446f09a5")
        (revision "3"))
    (package
      (inherit guile-websocket)
      (version (string-append (package-version guile-websocket)
                              "-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.dthompson.us/guile-websocket.git")
                      (commit commit)))
                (sha256
                 (base32
                  "0qssn6jycpd6d2cnikbwj2rvzdmckm2lyy4wx9g2mjks6g181b3n"))))
      (inputs (list guile-3.0 guile-gnutls)))))

(define (keep-file? file stat)
  (not (any (lambda (my-string)
              (string-contains file my-string))
            (list ".git" ".dir-locals.el" "guix.scm"))))

(package
  (name "guile-goblins")
  (version "0.15.0-git")
  (source (local-file (dirname (current-filename))
                      #:recursive? #t
                      #:select? keep-file?))
  (build-system gnu-build-system)
  (arguments
   `(#:phases
     (modify-phases %standard-phases
       (replace 'bootstrap
         (lambda _
           (invoke "autoreconf" "-vif"))))
     #:make-flags
     ,#~(list "GUILE_AUTO_COMPILE=0")))
  (native-inputs
   (list
     autoconf
     automake
     pkg-config
     texinfo))
  (inputs (list guile-3.0))
  (propagated-inputs
   (list guile-fibers guile-gnutls guile-websocket-next))
  (synopsis "Transactional, distributed object programming environment")
  (description
   "Spritely Goblins is a transactional, distributed object programming
environment following object capability principles.  This is the guile version
of the library!")
  (home-page "https://spritelyproject.org/")
  (license license:asl2.0))
