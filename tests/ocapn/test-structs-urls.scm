(define-module (tests ocapn test-structs-urls)
  #:use-module (goblins ocapn structs-urls)
  #:use-module (srfi srfi-64))

(test-begin "test-structs-urls")

(define ocapn-m1
  (make-ocapn-machine
   'fake
   "wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
   #f))

(define ocapn-m1*
  (make-ocapn-machine
   'fake
   "wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
   #t))

(define ocapn-m2
  (make-ocapn-machine
   'fake
   "yupy8klbgvtxwopxz93oyx5rxtglasaphptdjbb0hqjfvsalsinc9p7g"
   #f))

(define ocapn-sref1
  (make-ocapn-sturdyref ocapn-m1 "foobar"))

(define ocapn-c1
  (make-ocapn-cert ocapn-m1 "foobar"))

(define ocapn-bu1
  (make-ocapn-bearer-union ocapn-c1 'type-of-key "i-am-a-private-key"))

(test-assert
    "Verify ocapn-machine? tests positive when given an ocapn-machine"
  (ocapn-machine? ocapn-m1))

;; ocapn-struct->ocapn-machine
(test-assert
    "ocapn-struct->ocapn-machine with an ocapn-machine"
  (equal? (ocapn-struct->ocapn-machine ocapn-m1)
	  ocapn-m1))

(test-assert
    "ocapn-struct->ocapn-machine with an ocapn-studyref"
  (equal? (ocapn-struct->ocapn-machine ocapn-sref1)
	  ocapn-m1))

(test-assert
    "ocapn-struct->ocapn-machine with an ocapn-cert"
  (equal? (ocapn-struct->ocapn-machine ocapn-c1)
	  ocapn-m1))

(test-assert
    "ocapn-struct->ocapn-machine with an ocapn-cert"
  (equal? (ocapn-struct->ocapn-machine ocapn-bu1)
	  ocapn-m1))

;; same-machine-location?
(test-assert
    "same-machine-location? with the same machine, and same hints"
  (same-machine-location? ocapn-m1 ocapn-m1))

(test-assert
    "same-machine-location? with the same machine, but different hints"
  (same-machine-location? ocapn-m1 ocapn-m1*))

(test-assert
    "same-machine-location? doesn't match with two different machines"
  (not (same-machine-location? ocapn-m1 ocapn-m2)))

;; uri->ocapn-*
(test-assert
    "Verify uri->ocapn-machine produces the correct ocapn-machine"
  (equal? (uri->ocapn-machine "ocapn:m.fake.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd")
	  ocapn-m1))

(test-assert
    "Verify uri->ocapn-sturdyref produces the correct ocapn-studyref"
  (equal? (uri->ocapn-sturdyref "ocapn:m.fake.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd/foobar")
	  ocapn-sref1))

(test-assert
    "Verify uri->ocapn-cert produces the correct ocapn-cert"
  (equal? (uri->ocapn-cert "ocapn:m.fake.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd/foobar")
	  ocapn-c1))

(test-assert
    "Verify uri->ocapn-bearer-union produces the correct ocapn-bearer-union"
  (equal? (uri->ocapn-bearer-union
	   (string-append
	    "ocapn:m.fake.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd" ;; machine
	    "/"
	    "foobar" ;; cert
	    "/"
	    "type-of-key" ;; type of key
	    "."
	    "i-am-a-private-key")) ;; private key
	  ocapn-bu1))

(test-end "test-structs-urls")
