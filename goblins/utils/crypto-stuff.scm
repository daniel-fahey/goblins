;; Where we link in cryptography functions.  This might be swappable
;; in the future, dunno.

(define-module (goblins utils crypto-stuff)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt pk-crypto)
  #:use-module (gcrypt random)
  #:use-module (gcrypt base64)
  #:use-module (rnrs bytevectors)
  #:export (sha256d
            strong-random-bytes
            url-base64-encode))

(define (sha256d input)
  (sha256 (sha256 input)))

(define (strong-random-bytes byte-size)
  (gen-random-bv 32 %gcry-strong-random))

(define (url-base64-encode bv)
  (base64-encode bv 0 (bytevector-length bv) #f #t base64url-alphabet))
