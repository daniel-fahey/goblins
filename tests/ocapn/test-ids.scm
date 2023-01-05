(define-module (tests ocapn test-ids)
  #:use-module (goblins ocapn ids)
  #:use-module (srfi srfi-64))

(test-begin "test-ids")

(define ocapn-m1
  (make-ocapn-machine
   'fake
   "4wy6gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
   #f))

(define ocapn-m1*
  (make-ocapn-machine
   'fake
   "4wy6gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
   #t))

(define ocapn-m2
  (make-ocapn-machine
   'fake
   "8upy8klbgvtxwopxz93oyx5rxtglasaphptdjbb0hqjfvsalsinc9p7g"
   #f))

(define ocapn-sref1
  (make-ocapn-sturdyref ocapn-m1 #vu8(74 174 136 226 211 114 92 53 153 139 168 28 82 26 52 183 107 50 123 83 116 61 247 240 172 189 77 35 75 63 51 162)))

(test-assert
    "Verify ocapn-machine? tests positive when given an ocapn-machine"
  (ocapn-machine? ocapn-m1))

;; ocapn-id->ocapn-machine
(test-assert
    "ocapn-id->ocapn-machine with an ocapn-machine"
  (equal? (ocapn-id->ocapn-machine ocapn-m1)
          ocapn-m1))

(test-assert
    "ocapn-id->ocapn-machine with an ocapn-studyref"
  (equal? (ocapn-id->ocapn-machine ocapn-sref1)
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

;; Check string->ocapn-id
(test-assert
    "Verify string->ocapn-id produces the correct ocapn-machine"
  (equal? (string->ocapn-id "ocapn://4wy6gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd.fake")
          ocapn-m1))

(test-assert
    "Verify string->ocapn-id produces the correct ocapn-studyref"
  (equal? (string->ocapn-id "ocapn://4wy6gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd.fake/s/Sq6I4tNyXDWZi6gcUho0t2sye1N0PffwrL1NI0s_M6I")
          ocapn-sref1))

;; Check ocapn-id->string
(test-assert
    "ocapn-id->string works for ocapn-machine"
  (string=?
   (ocapn-id->string ocapn-m1)
   "ocapn://4wy6gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd.fake"))

(test-assert
    "ocapn-id->string works for ocapn-studyref"
  (string=?
   (ocapn-id->string ocapn-sref1)
   "ocapn://4wy6gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd.fake/s/Sq6I4tNyXDWZi6gcUho0t2sye1N0PffwrL1NI0s_M6I"))

(test-end "test-ids")
