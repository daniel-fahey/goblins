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
  (guix packages)
  ((guix licenses) #:prefix license:)
  (guix download)
  (guix build-system gnu)
  (gnu packages)
  (gnu packages autotools)
  (gnu packages gnupg)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo))

(package
  (name "guile-goblins")
  (version "0.6-pre")
  (source "./guile-goblins-0.6-pre.tar.gz")
  (build-system gnu-build-system)
  (arguments `())
  (native-inputs
    `(;; just for environments for local hacking
      ("guile-hall" ,guile-hall)
      ;; these are actually native-inputs for this package :P
      ("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)
      ("texinfo" ,texinfo)))
  (inputs `(("guile" ,guile-3.0)))
  (propagated-inputs
    `(("guile-fibers" ,guile-fibers)
      ("guile-gcrypt" ,guile-gcrypt)))
  (synopsis
    "A transactional, distributed object programming environment")
  (description
    "Spritely Goblins is a transactional, distributed object programming
environment following object capability principles.  This is the guile version
of the library!")
  (home-page "https://spritelyproject.org/")
  (license license:asl2.0))

