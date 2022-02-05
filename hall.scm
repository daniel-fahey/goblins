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
   `(("guile-hall" ,guile-hall)
     ("guile-fibers" (fibers) ,guile-fibers)))
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
                  (scheme-file "nonce-registry")
                  (scheme-file "cell")
                  (scheme-file "common")
                  (scheme-file "ward")
                  (scheme-file "swappable")))
               (scheme-file "ghash")
               (scheme-file "vat")
               (scheme-file "vrun")
               (scheme-file "core")
               (scheme-file "inbox")))))
         (tests ((directory
                   "tests"
                   ((directory
                      "utils"
                      ((scheme-file "test-bytes-stuff")))
                    (directory
                      "actor-lib"
                      ((scheme-file "test-ward")
                       (scheme-file "test-common")
                       (scheme-file "test-swappable")
                       (scheme-file "test-cell")))
                    (scheme-file "test-inbox")
                    (scheme-file "test-ghash")
                    (scheme-file "test-await")
                    (scheme-file "test-core")))))
         (programs ((directory "scripts" ())))
         (documentation
           ((org-file "README")
            (symlink "README" "README.org")
            (symlink "HACKING" "README.org")
            (symlink "COPYING" "LICENSE.txt")
            (directory "doc" ((texi-file "goblins")))
            (text-file "AUTHORS")))
         (infrastructure
           ((scheme-file "guix") (scheme-file "hall")))))
