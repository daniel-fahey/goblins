;; Where we link in cryptography functions.  This might be swappable
;; in the future, dunno.

(define-module (goblins utils crypto-stuff)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt pk-crypto)
  #:use-module (gcrypt random)
  #:export (sha256d
            strong-random-bytes))

(define (sha256d input)
  (sha256 (sha256 input)))

(define (strong-random-bytes byte-size)
  (gen-random-bv 32 %gcry-strong-random))
