(use-modules (rnrs bytevectors)
             ((gcrypt pk-crypto)
              #:prefix gcrypt:pk-crypto:)
             (gcrypt hash)
             (gcrypt random)
             (ice-9 match)
             ((scheme base)
              #:select (bytevector-append)))

(define (data->canonical-sexp data)
  (gcrypt:pk-crypto:sexp->canonical-sexp
   `(data (flags eddsa) (hash-algo sha512)
          (value ,data))))

`(("uint8Array" . (("new"    . ,make-bytevector)
                   ("length" . ,bytevector-length)
                   ("ref"    . ,bytevector-u8-ref)
                   ("set"    . ,bytevector-u8-set!)))
  ("crypto" .
   (("digest" .
     ;; We only use digest for sha256, so just do sha256
     ,(lambda (algorithm data)
        (sha256 data)))
    ("getRandomValues" .
     ,(lambda (array)
        (gen-random-bv (bytevector-length array))))
    ("generateEd25519KeyPair" .
     ,(lambda ()
        (gcrypt:pk-crypto:generate-key
         (gcrypt:pk-crypto:sexp->canonical-sexp
          '(genkey (eddsa (curve Ed25519)
                          (flags eddsa)))))))
    ("keyPairPrivateKey" .
     ,(lambda (keyPair)
        (gcrypt:pk-crypto:find-sexp-token
         keyPair
         'private-key)))
    ("keyPairPublicKey" .
     ,(lambda (keyPair)
        (match
            (gcrypt:pk-crypto:canonical-sexp->sexp
             (gcrypt:pk-crypto:find-sexp-token
              keyPair
              'public-key))
          (`(public-key
             (ecc (curve Ed25519)
                  (flags eddsa)
                  (q ,key)))
           key))))
    ("exportKey" . ,(lambda (key) key))
    ("importPublicKey" .
     ,(lambda (key)
        (gcrypt:pk-crypto:sexp->canonical-sexp
         `(public-key
           (ecc (curve Ed25519)
                (flags eddsa)
                (q ,key))))))
    ("signEd25519" .
     ,(lambda (data privateKey)
        (match (gcrypt:pk-crypto:canonical-sexp->sexp
                (gcrypt:pk-crypto:sign
                 (data->canonical-sexp data)
                 privateKey))
          (`(sig-val (eddsa (r ,bv-begin)
                            (s ,bv-end)))
           (bytevector-append bv-begin bv-end)))))
    ("verifyEd25519" .
     ,(lambda (signature data publicKey)
        (gcrypt:pk-crypto:verify
         signature
         (data->canonical-sexp data)
         publicKey))))))
