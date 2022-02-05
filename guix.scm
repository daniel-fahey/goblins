(use-modules
  (guix packages)
  ((guix licenses) #:prefix license:)
  (guix download)
  (guix build-system gnu)
  (gnu packages)
  (gnu packages autotools)
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
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)
      ("texinfo" ,texinfo)))
  (inputs `(("guile" ,guile-3.0)))
  (propagated-inputs `())
  (synopsis
    "A transactional, distributed object programming environment")
  (description
    "Spritely Goblins is a transactional, distributed object programming
environment following object capability principles.  This is the guile version
of the library!")
  (home-page "https://spritelyproject.org/")
  (license license:asl2.0))

