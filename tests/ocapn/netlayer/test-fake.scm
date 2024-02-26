(define-module (tests ocapn netlayer test-fake)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer fake)
  #:use-module (tests utils)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (srfi srfi-64))

(test-begin "test-fake-netlayer")

(define test-vat (spawn-vat #:name "test-vat"))
(define test-channel (make-channel))

;; Tests for the ^fake-network
(define test-network
  (with-vat test-vat
    (spawn ^fake-network)))

(test-error
 "Check connecting to a non-existing network fails"
 #t
 (with-vat test-vat
   ($ test-network 'connect-to "does-not-exist")))

(with-vat test-vat
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
        (channel? (car (cdr (cdr message)))))))

;; Tests for the ^fake-netlayer
(define a-vat (spawn-vat #:name "a-vat"))
(define b-vat (spawn-vat #:name "b-vat"))
(define a-new-conn-ch (make-channel))
(define b-new-conn-ch (make-channel))
(define a-location (string->ocapn-id "ocapn://a.fake"))
(define b-location (string->ocapn-id "ocapn://b.fake"))

(define a-netlayer
  (with-vat a-vat
    (spawn ^fake-netlayer "a" test-network a-new-conn-ch)))
(define b-netlayer
  (with-vat b-vat
    (spawn ^fake-netlayer "b" test-network b-new-conn-ch)))

(with-vat test-vat
 ($ test-network 'register "a" a-new-conn-ch)
 ($ test-network 'register "b" b-new-conn-ch))

(define a-mycapn
  (with-vat a-vat (spawn-mycapn a-netlayer)))
(define b-mycapn
  (with-vat b-vat (spawn-mycapn b-netlayer)))

(define (^greeter _bcom my-name)
  (lambda (your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name)))

(define alice
  (with-vat a-vat
    (spawn ^greeter "Alice")))
(define bob
  (with-vat b-vat
    (spawn ^greeter "Bob")))

(define alice-locator-sref
  (with-vat a-vat
    ($ a-mycapn 'register alice 'fake)))
(define bob-locator-sref
  (with-vat b-vat
    ($ b-mycapn 'register bob 'fake)))



(let ((result
       (resolve-vow-and-return-result
        a-vat
        (lambda ()
          (define bob-vow (<- a-mycapn 'enliven bob-locator-sref))
          (<- bob-vow "Arthur")))))
  (test-equal "Able to enliven a far sturdyref and using it from a->b"
    #(ok "Hello Arthur, my name is Bob!")
    result))

(let ((result
       (resolve-vow-and-return-result
        b-vat
        (lambda ()
          (define alice-vow (<- b-mycapn 'enliven alice-locator-sref))
          (<- alice-vow "Ben")))))
  (test-equal "Able to enliven a far sturdyref and using it form b->a"
    #(ok "Hello Ben, my name is Alice!")
    result))

(test-end "test-fake-netlayer")
