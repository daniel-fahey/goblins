(define-module (goblins test-vat)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64))

(test-begin "test-vat")

(define a-vat (spawn-vat))
(define-vat-run a-run a-vat)

(define (^friendo _bcom)
  (lambda ()
    'hello))

(define my-friend
  (a-vat
   (lambda () (spawn ^friendo))))

(test-eq
    "Check define-vat-run works"
  'hello
  (a-run ($ my-friend)))

(define (^counter bcom n)
  (lambda ()
    (bcom (^counter bcom (+ n 1)) n)))

(define a-counter
  (a-vat
   (lambda () (spawn ^counter 0))))

(define (run vat op . rest)
  (vat (lambda () (apply op rest))))

(test-eq (run a-vat $ a-counter) 0)
(test-eq (run a-vat $ a-counter) 1)
(test-eq (run a-vat $ a-counter) 2)
(test-eq (run a-vat $ a-counter) 3)
(run a-vat <-np a-counter)
(usleep 1)
(test-eq (run a-vat $ a-counter) 5)

(define (^counter-poker _bcom counter)
  (lambda ()
    (<-np counter)))
(define counter-poker
  (run a-vat spawn ^counter-poker a-counter))
(test-eq (run a-vat $ a-counter) 6)
(run a-vat $ counter-poker)
(usleep 500)
(test-eq (run a-vat $ a-counter) 8)
(run a-vat $ counter-poker)
(usleep 500)
(test-eq (run a-vat $ a-counter) 10)

;; Inter-vat communication
(define b-vat (spawn-vat))
(run b-vat <- a-counter)
(usleep 500)
(test-eq (run a-vat $ a-counter) 12)

;; Check inter-vat promise resolution
(let ((set-this #f))
  (b-vat
   (lambda ()
     (on (<- my-friend)
	 (lambda (response)
	   (set! set-this (format #f "I got: ~a" response))))))
  (usleep 500)
  (test-equal
      "Check promise resolution using on between vats"
    set-this
    "I got: hello"))

;; Promise pipelining test
(define (^car-factory _bcom)
  (lambda (color)
    (define (^car _bcom)
      (lambda ()
	(format #f "The ~a car says: *vroom vroom*!" color)))
    (spawn ^car)))
(define car-factory (run a-vat spawn ^car-factory))
(let ((car-result-here #f))
  (a-vat
   (lambda ()
     (define car-vow (<- car-factory 'green))
     (on (<- car-vow)
	 (lambda (car-says)
	   (set! car-result-here car-says)))))
  (usleep 500)
  (test-equal
      "Check basic promise pipelining on the same vat works"
    car-result-here
    "The green car says: *vroom vroom*!"))

;; Check promise pipelining between vats
(let ((car-result-here #f))
  (b-vat
   (lambda ()
     (define car-vow (<- car-factory 'red))
     (on (<- car-vow)
	 (lambda (car-says)
	   (set! car-result-here car-says)))))
  (usleep 500)
  (test-equal
      "Check that basic promise pipeling works between vats"
    car-result-here
    "The red car says: *vroom vroom*!"))

;; Test promise pipeling with a broken promise.
(define (^borked-factory _bcom)
  (define (^car _bcom)
    (lambda ()
      (format #f "Vroom vroom")))

  (match-lambda
    ('make-car (spawn ^car))
    ('make-error (error "Oops! no vrooming here :("))))

(define (make-car vat factory method-name)
  (let ((result #f)
	(is-borked? #f))
    (vat
     (lambda ()
       (define car-vow
	 (on (<- factory method-name)
	     (lambda (car)
	       car)
	     #:catch
	     (lambda (some-error)
	       (set! is-borked? #t))
	     #:promise? #t))
       (on (<- car-vow)
	   (lambda (car-says)
	     (set! result car-says)))))
    (usleep 500)
    (values result is-borked?)))

(define borked-factory (run a-vat spawn ^borked-factory))

;; Check the initial working car.
(let-values (((result is-borked?)
	     (make-car a-vat borked-factory 'make-car)))
  (test-assert
      "Sanity check to make sure broked-car factory normally works"
    (and (string=? result "Vroom vroom")
	 (not is-borked?))))

;; Now check the error.
(let-values (((result is-borked?)
	      (make-car a-vat borked-factory 'make-error)))
  (test-assert
      "Check promise pipeling breaks on error on the same vat"
    (and (not result)
	 is-borked?)))

;; Now check that errors work across vats
(let-values (((result is-borked?)
	      (make-car b-vat borked-factory 'make-error)))
  (test-assert
      "Check promise pipeling breaks on error between vats"
    (and (not result)
	 is-borked?)))

(test-end "test-vat")
