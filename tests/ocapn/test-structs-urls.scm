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
  (make-ocapn-sturdyref ocapn-m1 #vu8(74 174 136 226 211 114 92 53 153 139 168 28 82 26 52 183 107 50 123 83 116 61 247 240 172 189 77 35 75 63 51 162)))

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
  (equal? (string->ocapn-uri "ocapn://m.fake.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd")
          ocapn-m1))

(test-assert
    "Verify uri->ocapn-sturdyref produces the correct ocapn-studyref"
  (equal? (string->ocapn-uri "ocapn://s.fake.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd/Sq6I4tNyXDWZi6gcUho0t2sye1N0PffwrL1NI0s_M6I")
          ocapn-sref1))

(test-end "test-structs-urls")
