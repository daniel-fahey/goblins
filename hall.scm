(hall-description
  (name "goblins")
  (prefix "guile")
  (version "0.1-pre")
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
  (dependencies `())
  (files (libraries
           ((scheme-file "goblins")
            (directory "goblins" ())))
         (tests ((directory
                   "tests"
                   ((scheme-file "test-inbox")
                    (scheme-file "test-ghash")
                    (scheme-file "test-core")
                    (scheme-file "test-await")))))
         (programs ((directory "scripts" ())))
         (documentation
           ((org-file "README")
            (symlink "README" "README.org")
            (text-file "HACKING")
            (text-file "COPYING")
            (directory "doc" ((texi-file "goblins")))))
         (infrastructure
           ((scheme-file "guix")
            (text-file ".gitignore")
            (scheme-file "hall")))))
