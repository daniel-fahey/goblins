(define-module (goblins test-vat)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (srfi srfi-64))

(test-begin "test-vat")

(define a-vat (spawn-vat))
(define-vat-run a-vat-run a-vat)

(define (^friendo _bcom)
  (lambda ()
    'hello))

(define my-friend
  (a-vat
   (lambda () (spawn ^friendo))))

(test-eq
    "Check define-vat-run works"
  'hello
  (a-vat-run ($ my-friend)))

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
(sleep 1)
(test-eq (run a-vat $ a-counter) 5)

(define (^counter-poker _bcom counter)
  (lambda ()
    (<-np counter)))
(define counter-poker
  (run a-vat spawn ^counter-poker a-counter))
(test-eq (run a-vat $ a-counter) 6)
(run a-vat $ counter-poker)
(sleep 1)
(test-eq (run a-vat $ a-counter) 8)
(run a-vat $ counter-poker)
(sleep 1)
(test-eq (run a-vat $ a-counter) 10)

;; Inter-vat communication
(define b-vat (spawn-vat))
(run b-vat <- a-counter)
(sleep 1)
(test-eq (run a-vat $ a-counter) 12)

(test-end "test-vat")
