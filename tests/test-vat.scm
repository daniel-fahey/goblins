(define-module (goblins test-vat)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (srfi srfi-64))

(test-begin "test-vat")

(define vat (spawn-vat))
(define-vat-run vat-run vat)

(define (^friendo _bcom)
  (lambda ()
    'hello))

(define my-friend
  (vat
   'run
   (lambda () (spawn ^friendo))))

(test-eq
    "Check define-vat-run works"
  'hello
  (vat-run ($ my-friend)))

(define (^counter bcom n)
  (lambda ()
    (bcom (^counter bcom (+ n 1)) n)))

(define a-counter
  (vat
   'run
   (lambda () (spawn ^counter 0))))

(define (get-counter op counter)
  (vat 'run (lambda () (op counter))))

(test-eq (get-counter $ a-counter) 0)
(test-eq (get-counter $ a-counter) 1)
(test-eq (get-counter $ a-counter) 2)
(test-eq (get-counter $ a-counter) 3)
(get-counter <-np a-counter)
(sleep 1)
(test-eq (get-counter $ a-counter) 5)

(define (^counter-poker _bcom counter)
  (lambda ()
    (<-np counter)))
(define counter-poker
  (vat 'run (lambda () (spawn ^counter-poker a-counter))))
(test-eq (get-counter $ a-counter) 6)
(vat 'run (lambda () ($ counter-poker)))
(sleep 1)
(test-eq (get-counter $ a-counter) 8)
(vat 'run (lambda () ($ counter-poker)))
(sleep 1)
(test-eq (get-counter $ a-counter) 10)

;; Inter-vat communication
(define another-vat (spawn-vat))
(another-vat
 'run
 (lambda () (<-np a-counter)))
(sleep 1)
(test-eq (get-counter $ a-counter) 12)

(test-end "test-vat")
