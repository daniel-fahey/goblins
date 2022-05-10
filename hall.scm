;; This file is waived into the public domain, as-is, no warranty provided.
;;
;; If the public domain doesn't exist where you live, consider
;; this a license which waives all copyright and neighboring intellectual
;; restrictions laws mechanisms, to the fullest extent possible by law,
;; as-is, no warranty provided.
;;
;; No attribution is required and you are free to copy-paste and munge
;; into your own project.

(hall-description
  (name "goblins")
  (prefix "guile")
  (version "0.6-pre")
  (author "Christine Lemmer-Webber")
  (copyright (2022))
  (synopsis
    "A transactional, distributed object programming environment")
  (description
    "Spritely Goblins is a transactional, distributed object programming
environment following object capability principles.  This is the guile version
of the library!")
  (home-page "https://spritelyproject.org/")
  (license asl2.0)
  (dependencies
   `(("guile-fibers" (fibers) ,guile-fibers)
     ("guile-gcrypt" (gcrypt hash) ,guile-gcrypt)))
  (files (libraries
           ((scheme-file "goblins")
            (directory
              "goblins"
              ((directory
                 "ocapn"
                 ((scheme-file "crypto-stubs")
                  (scheme-file "define-recordable")
                  (scheme-file "structs-urls")
                  (scheme-file "captp")))
               (directory
                 "utils"
                 ((scheme-file "simple-sealers")
                  (scheme-file "bytes-stuff")
                  (scheme-file "crypto-stuff")
                  (scheme-file "weak-box")
                  (scheme-file "assert-type")
                  (scheme-file "simple-dispatcher")))
               (directory "contrib" ((scheme-file "syrup")))
               (directory
                 "actor-lib"
                 ((scheme-file "methods")
                  (scheme-file "ticker")
                  (scheme-file "nonce-registry")
                  (scheme-file "cell")
                  (scheme-file "common")
                  (scheme-file "ward")
                  (scheme-file "swappable")))
               (scheme-file "ghash")
               (scheme-file "vat")
               (scheme-file "default-vat-scheduler")
               (scheme-file "vrun")
               (scheme-file "core")
               (scheme-file "base-io-ports")
               (scheme-file "inbox")))))
         (tests ((directory
                   "tests"
                   ((directory
                      "utils"
                      ((scheme-file "test-bytes-stuff")))
                    (directory
                      "actor-lib"
                      ((scheme-file "test-ticker")
                       (scheme-file "test-ward")
                       (scheme-file "test-common")
                       (scheme-file "test-swappable")
                       (scheme-file "test-cell")))
                    (scheme-file "test-inbox")
                    (scheme-file "test-ghash")
                    (scheme-file "test-await")
                    (scheme-file "test-core")))))
         (programs ())
         (documentation
           ((org-file "README")
            (symlink "README" "README.org")
            (symlink "HACKING" "README.org")
            (symlink "COPYING" "LICENSE.txt")
            (directory
              "doc"
              ((org-file "goblins") (texi-file "goblins")))
            (text-file "AUTHORS")
            (text-file "NEWS")
            (text-file "AUTHORS")
            (text-file "ChangeLog")))
         (infrastructure
           ((scheme-file "guix") (scheme-file "hall")))))
