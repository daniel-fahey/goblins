(define-module (goblins test-vat)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib methods)
  #:use-module (tests utils)
  #:use-module (fibers)
  #:use-module (fibers channels)
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
(resolve-vow-and-return-result
 a-vat
 (lambda () (<- a-counter)))
(test-eq (run a-vat $ a-counter) 5)

(define (^counter-poker _bcom counter)
  (lambda ()
    (<-np counter)))
(define counter-poker
  (run a-vat spawn ^counter-poker a-counter))
(test-eq (run a-vat $ a-counter) 6)
(run a-vat $ counter-poker)
(usleep 50000)
(test-eq (run a-vat $ a-counter) 8)
(run a-vat $ counter-poker)
(usleep 50000)
(test-eq (run a-vat $ a-counter) 10)

;; Inter-vat communication
(define b-vat (spawn-vat))
(let ((result
       (resolve-vow-and-return-result
        b-vat
        (lambda () (<- a-counter)))))
  (test-eq (run a-vat $ a-counter) 12))

;; Check inter-vat promise resolution
(let ((result
       (resolve-vow-and-return-result
        b-vat
        (lambda () (<- my-friend)))))
  (test-assert
      "Check promise resolution using on between vats"
    (match result
      (#('ok 'hello) #t)
      (_ #f))))

;; Promise pipelining test
(define (^car-factory _bcom)
  (lambda (color)
    (define (^car _bcom)
      (lambda ()
        (format #f "The ~a car says: *vroom vroom*!" color)))
    (spawn ^car)))
(define car-factory (run a-vat spawn ^car-factory))
(let ((result
       (resolve-vow-and-return-result
        a-vat
        (lambda ()
          (define car-vow (<- car-factory 'green))
          (<- car-vow)))))
  (test-assert
      "Check basic promise pipelining on the same vat works"
    (match result
      (#('ok "The green car says: *vroom vroom*!") #t)
      (_ #f))))

;; Check promise pipelining between vats
(let ((result
       (resolve-vow-and-return-result
        b-vat
        (lambda ()
          (define car-vow (<- car-factory 'red))
          (<- car-vow)))))
  (test-assert
      "Check that basic promise pipeling works between vats"
    (match result
      (#('ok "The red car says: *vroom vroom*!") #t)
      (_ #f))))

;; Test promise pipeling with a broken promise.
(define (^borked-factory _bcom)
  (define (^car _bcom)
    (lambda ()
      (format #f "Vroom vroom")))

  (match-lambda
    ('make-car (spawn ^car))
    ('make-error (error "Oops! no vrooming here :("))))

(define (try-car-pipeline vat factory method-name)
  (resolve-vow-and-return-result
   vat
   (lambda ()
     (define car-vow
       (<- factory method-name))
     (<- car-vow))))

(define borked-factory (run a-vat spawn ^borked-factory))

;; Check the initial working car.
(let ((result (try-car-pipeline a-vat borked-factory 'make-car)))
  (test-assert
      "Sanity check to make sure factory normally works"
    (match result
      (#('ok "Vroom vroom") #t)
      (_ #f))))

(let ((result (try-car-pipeline b-vat borked-factory 'make-car)))
  (test-assert
      "Sanity check to make sure factory normally works across vats"
    (match result
      (#('ok "Vroom vroom") #t)
      (_ #f))))

;; Now check the error.
(let ((result (try-car-pipeline a-vat borked-factory 'make-error)))
  (test-assert
      "Check promise pipeling breaks on error on the same vat"
    (match result
      (#('err _err) #t)
      (_ #f))))

;; Now check that errors work across vats
(let ((result (try-car-pipeline b-vat borked-factory 'make-error)))
  (test-assert
      "Check promise pipeling breaks on error between vats"
    (match result
      (#('err _err) #t)
      (_ #f))))

;;; Literally the version from the Goblins docs

;; Create a "car factory", which makes cars branded with
;; company-name.
(define (^car-factory2 bcom company-name)
  ;; The constructor for cars we will create.
  (define (^car bcom model color)
    (methods                      ; methods for the ^car
     ((drive)                    ; drive the car
      (format #f "*Vroom vroom!*  You drive your ~a ~a ~a!"
              color company-name model))))
  ;; methods for the ^car-factory instance
  (methods                        ; methods for the ^car-factory
   ((make-car model color)       ; create a car
    (spawn ^car model color))))

(define fork-motors
  (a-vat
   (lambda ()
     (spawn ^car-factory2 "Fork"))))

(define car-vow
  (b-vat
   (lambda ()
     (<- fork-motors 'make-car "Explorist" "blue"))))

(define car-pipeline-result
  (resolve-vow-and-return-result
   b-vat
   (lambda ()
     (on (<- car-vow 'drive)       ; B->A: send message to future car
         (lambda (val)             ; A->B: result of that message
           (format #f "Heard: ~a\n" val))
         #:promise? #t))))

(test-equal "Make sure promise pipelining works, version 2"
  #(ok "Heard: *Vroom vroom!*  You drive your blue Fork Explorist!\n")
  car-pipeline-result)

(test-end "test-vat")
