#!/usr/bin/env -S guile -e main -s
!#

(use-modules (goblins)
             (goblins ocapn netlayer libp2p)
             (goblins ocapn captp)
             (goblins ocapn ids)
             (goblins persistence-store syrup)
             (goblins actor-lib cell)
             (fibers conditions)
             (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 curried-definitions))

(define-actor (^greeter _bcom my-name)
  (lambda (your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name)))

(define-actor (^greeter-sref bcom mycapn netlayer greeter
                             #:optional [swiss-num (spawn ^cell)])
  (lambda ()
    (if ($ swiss-num)
        (on (<- netlayer 'our-location)
            (lambda (our-location)
              (make-ocapn-sturdyref our-location ($ swiss-num)))
            #:promise? #t)
        (on (<- mycapn 'register greeter ($ netlayer 'netlayer-name))
            (lambda (sref)
              ($ swiss-num (ocapn-sturdyref-swiss-num sref))
              sref)
            #:promise? #t))))

(define env
  (make-persistence-env
   `((((examples try-captp-onion) ^greeter) ,^greeter)
     (((examples try-captp-onion) ^greeter-sref) ,^greeter-sref))
   #:extends (list captp-env libp2p-netlayer-env)))

(define (libp2p-server)
  (define-values (vat libp2p-netlayer libp2p-mycapn alice-sref)
    (spawn-persistent-vat
     env
     (lambda ()
       (define libp2p-netlayer
         (spawn ^libp2p-netlayer))
       (define libp2p-mycapn
         (spawn-mycapn libp2p-netlayer))
       (define alice
         (spawn ^greeter "Alice"))
       (define alice-sref
         (spawn ^greeter-sref libp2p-mycapn libp2p-netlayer alice))
       (values libp2p-netlayer libp2p-mycapn alice-sref))
     (make-syrup-store "/tmp/libp2p-netlayer.syrup")))
  
  (with-vat vat
    (on (<- alice-sref)
        (lambda (sref)
          (format #t "Connect to: ~a\n" (ocapn-id->string sref)))))
  (wait (make-condition)))

(define (libp2p-client greeter-sref-arg)
  (define vat
    (spawn-vat))
  (define libp2p-netlayer
    (with-vat vat
      (spawn ^libp2p-netlayer)))
  (define mycapn
    (with-vat vat
      (spawn-mycapn libp2p-netlayer)))
  (define stop-condition (make-condition))
  (with-vat vat
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
  (match args
    [(_cmd)
     (libp2p-server)]
    [(_cmd alice-sref)
     (libp2p-client alice-sref)]
    [something-else
     (error "wrong number of arguments given, got ~a" something-else)]))

(main (command-line))
