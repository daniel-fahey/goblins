#!/usr/bin/env -S guile -e main -s
!#

(use-modules (goblins)
             (goblins ocapn netlayer onion)
             (goblins ocapn netlayer fake)
             (goblins ocapn captp)
             (goblins ocapn ids)
             (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 curried-definitions))

(define ((^greeter _bcom my-name) your-name)
  (format #f "Hello ~a, my name is ~a!" your-name my-name))

(define* (setup-tor-mycapn #:optional tor-onion-pair)
  (define vat (spawn-vat #:name 'ocapn))
  (define onion-netlayer
    (vat
     (lambda ()
       (match tor-onion-pair
         ((service-id . private-key)
          (restore-onion-netlayer service-id private-key))
         (#f (new-onion-netlayer))))))
  (define mycapn
    (vat
     (lambda ()
       (spawn-mycapn onion-netlayer))))
  (values vat onion-netlayer mycapn))

(define* (tor-server #:key (greeter-name "Alice")
                     tor-onion-pair)
  (define-values (machine-vat onion-netlayer mycapn)
    (setup-tor-mycapn tor-onion-pair))
  (define alice
    (machine-vat
     (lambda () (spawn ^greeter greeter-name))))
  (define alice-sref
    (machine-vat
     (lambda ()
       ($ mycapn 'register alice 'onion))))
  (values machine-vat onion-netlayer mycapn alice alice-sref))

(use-modules (fibers conditions))
(use-modules (goblins ocapn netlayer utils)
             (goblins ocapn netlayer onion-socks))

(define default-tor-socks-path
  (@@ (goblins ocapn netlayer onion) default-tor-socks-path))

(define (main args)
  ;; (define a-service-id "gwe2ammpmv4hh5ehkmfeltuz6e4ih6yqsdorxaps7xbb2mk6gf73v2qd")
  ;; (define a-private-key "ED25519-V3:WDAVx9zQh7Eh4mqbM6izGIF43spjKtS9sMd+pj1XTUICiUz4+slw3w8LZXJXKsKYFRXdNYUGh7GCXcwgIdvFQg==")
  (define-values (a-machine-vat a-onion-netlayer a-mycapn alice alice-sref)
    (tor-server ;; #:tor-onion-pair
                ;; (cons a-service-id a-private-key)
                ))

  (format #t "Connect to: ~a\n" (ocapn-id->string alice-sref))

  (wait (make-condition)))
