#!/usr/bin/env -S guile -e main -s
!#

(use-modules (goblins)
             (goblins ocapn netlayer onion)
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
   #:extends (list captp-env onion-netlayer-env)))

(define (onion-server)
  (define-values (vat onion-netlayer onion-mycapn alice-sref)
    (spawn-persistent-vat
     env
     (lambda ()
       (define onion-netlayer
         (spawn ^onion-netlayer))
       (define onion-mycapn
         (spawn-mycapn onion-netlayer))
       (define alice
         (spawn ^greeter "Alice"))
       (define alice-sref
         (spawn ^greeter-sref onion-mycapn onion-netlayer alice))
       (values onion-netlayer onion-mycapn alice-sref))
     (make-syrup-store "onion-netlayer.syrup")))
  
  (with-vat vat
    (on (<- alice-sref)
        (lambda (sref)
          (format #t "Connect to: ~a\n" (ocapn-id->string sref)))))
  (wait (make-condition)))

(define (onion-client greeter-sref-arg)
  (define vat
    (spawn-vat))
  (define onion-netlayer
    (with-vat vat
      (spawn ^onion-netlayer)))
  (define mycapn
    (with-vat vat
      (spawn-mycapn onion-netlayer)))
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
     (onion-server)]
    [(_cmd alice-sref)
     (onion-client alice-sref)]
    [something-else
     (error "wrong number of arguments given, got ~a" something-else)]))

(main (command-line))
