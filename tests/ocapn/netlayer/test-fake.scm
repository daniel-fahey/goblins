(define-module (tests ocapn netlayer test-fake)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn structs-urls)
  #:use-module (goblins ocapn netlayer fake)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (srfi srfi-64))

(test-begin "test-fake-netlayer")

(define test-vat (spawn-vat))
(define test-channel (make-channel))

;; Tests for the ^fake-network
(define test-network
  (test-vat 'run (lambda () (spawn ^fake-network))))

(test-error
 "Check connecting to a non-existing network fails"
 #t
 (test-vat 'run (lambda () ($ test-network 'connect-to "does-not-exist"))))

(test-vat
 'run
 (lambda ()
   ($ test-network 'register "test" test-channel)
   (define test-connection ($ test-network 'connect-to "test"))

   (test-assert "Check we're getting back fibers channels"
     (and (eq? (car test-connection) '*outgoing-new-conn*)
          (channel? (car (cdr test-connection)))
          (channel? (car (cdr (cdr  test-connection))))))

   (define message (get-message test-channel))
   (test-assert
       "Check we're getting back two fibers channels"
     (and (eq? (car message) '*incoming-new-conn*)
          (channel? (car (cdr message)))
          (channel? (car (cdr (cdr message))))))))

;; Tests for the ^fake-netlayer
(define a-vat (spawn-vat))
(define b-vat (spawn-vat))
(define a-new-conn-ch (make-channel))
(define b-new-conn-ch (make-channel))
(define a-location (uri->ocapn-machine "ocapn:m.fake.a"))
(define b-location (uri->ocapn-machine "ocapn:m.fake.b"))

(define a-netlayer
  (a-vat (lambda () (spawn ^fake-netlayer "a" test-network a-new-conn-ch))))
(define b-netlayer
  (b-vat (lambda () (spawn ^fake-netlayer "b" test-network b-new-conn-ch))))

(test-vat
 (lambda ()
   ($ test-network 'register "a" a-new-conn-ch)
   ($ test-network 'register "b" b-new-conn-ch)))

(define a-mycapn
  (a-vat (lambda () (spawn-mycapn (list a-netlayer)))))
(define b-mycapn
  (b-vat (lambda () (spawn-mycapn (list b-netlayer)))))

(define a->b-vow
  (a-vat
   (lambda ()
     ($ a-mycapn 'connect-to-machine b-location))))
(define b->a-vow
  (b-vat
   (lambda ()
     ($ b-mycapn 'connect-to-machine a-location))))

(define (^greeter _bcom my-name)
  (lambda (your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name)))

(define alice
  (a-vat (lambda () (spawn ^greeter "Alice"))))
(define bob
  (b-vat (lambda () (spawn ^greeter "Bob"))))

(define alice-locator-sref
  (a-vat (lambda () ($ a-mycapn 'register alice 'fake))))
(define bob-locator-sref
  (b-vat (lambda () ($ b-mycapn 'register bob 'fake))))

(let ((result #f))
  (a-vat
   (lambda ()
     (define bob-vow (<- a-mycapn 'enliven bob-locator-sref))
     (on (<- bob-vow "Arthur")
     (lambda (response)
       (set! result `(fulfilled ,response)))
     #:catch
     (lambda (err)
       (set! result `(broken ,err))))))
  (sleep 2)
  (test-equal
      "Able to enliven a far sturdyref and using it"
    '(fulfilled "Hello Arthur, my name is Bob!")
    result))

;; TODO: port the final test over.

(test-end "test-fake-netlayer")
