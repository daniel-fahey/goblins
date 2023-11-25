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
  (define-values (onion-netlayer private-key service-id)
    (with-vat vat
     (match tor-onion-pair
       ((service-id . private-key)
        (restore-onion-netlayer private-key service-id))
       (#f (new-onion-netlayer)))))
  (define mycapn
    (with-vat vat (spawn-mycapn onion-netlayer)))
  (values vat onion-netlayer mycapn))

(define* (tor-server #:key (greeter-name "Alice")
                     tor-onion-pair)
  (define-values (node-vat onion-netlayer mycapn)
    (setup-tor-mycapn tor-onion-pair))
  (define alice
    (with-vat node-vat (spawn ^greeter greeter-name)))
  (define alice-sref
    (with-vat node-vat ($ mycapn 'register alice 'onion)))
  (values node-vat onion-netlayer mycapn alice alice-sref))

(use-modules (fibers conditions))
(use-modules (goblins ocapn netlayer utils)
             (goblins ocapn netlayer onion-socks))

(define (onion-server)
  (define-values (a-node-vat a-onion-netlayer a-mycapn alice alice-sref)
    (tor-server))
  (format #t "Connect to: ~a\n" (ocapn-id->string alice-sref))
  (wait (make-condition)))

(define (onion-client greeter-sref-arg)
  (define-values (onion-vat onion-netlayer mycapn)
    (setup-tor-mycapn))
  (define stop-condition (make-condition))
  (with-vat onion-vat
    (define greeter-sref (string->ocapn-id greeter-sref-arg))
    (define greeter-vow (<- mycapn 'enliven greeter-sref))
    (format #t "Connecting to alice on ~a, this can take a while.\n" greeter-sref-arg)
    (on (<- greeter-vow "Bob")
        (lambda (alice-said)
          (format #t "Alice said: ~a\n" alice-said)
          (signal-condition! stop-condition))))
  (wait stop-condition))

(define (main args)
  ;; If called with no arguments, we're the server, otherwise assume the argument
  ;; is a sturdyref to the greeter and message it.
  (cond [(= 1 (length args)) (onion-server)]
        [(= 2 (length args)) (onion-client (list-ref args 1))]
        [else (error "Wrong number of arguments given" args)]))
