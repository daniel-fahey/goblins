(define-module (goblins ocapn netlayer fake)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module ((goblins core) #:renamer (lambda (x) (if (eq? x '$) '$C x)))
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins ocapn structs-urls)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:export (^fake-network ^fake-netlayer))


(define (^fake-network _bcom)
  (define routes (spawn ^ghash))
  (methods
   [(register name new-conn-ch)
    ($C routes 'set name new-conn-ch)]
   [(connect-to name)
    (define connection-ch
      ($C routes 'ref name))
    (when (not (channel? connection-ch))
      (error (format #t "No connection found by name: ~a" name)))
    (define me->them
      (make-channel))
    (define them->me
      (make-channel))
    (spawn-fiber
     (lambda ()
       (put-message connection-ch (cons them->me me->them))))
    (cons me->them them->me)]))

(define (make-message-reader ch)
  (lambda (unmarshallers)
    ;; TODO: do we want to syrup encode, them?
    (get-message ch)))
(define (make-message-writer ch)
  (lambda (msg marshallers)
    (put-message ch msg)))

(define (^fake-netlayer _bcom our-name network new-conn-ch)
  (define our-location (make-ocapn-machine 'fake our-name #f))
  (define (start-listening conn-establisher)
    (define (listen)
      (define message-vow
	(spawn-fibrous-vow
	 (lambda () (get-message new-conn-ch))))
      (on message-vow
	  (lambda (ports)
	    (<- conn-establisher
		(make-message-reader (car ports))
		(make-message-writer (cdr ports))
		#t))
	  #:finally listen))
    (listen))

  (define (^netlayer bcom)
    (define base-beh
      (methods
       [(netlayer-name) 'fake]
       [(our-location) our-location]))

    (define pre-setup-beh
      (extend-methods
       base-beh
       [(setup conn-establisher)
	(start-listening conn-establisher)
	(bcom (ready-beh conn-establisher))]))

    (define (ready-beh conn-establisher)
      (extend-methods
       base-beh
       [(self-location? loc)
	(same-machine-location? our-location loc)]
       [(connect-to remote-machine)
	(match remote-machine
	  (($ <ocapn-machine> 'fake name #f)
	   (on (<- network 'connect-to name)
	       (lambda (ports)
		 (<- conn-establisher
		       (make-message-reader (car ports))
		       (make-message-writer (cdr ports))
		       #f))
	       #:promise? #t)))]))
    pre-setup-beh)
  (spawn ^netlayer))
