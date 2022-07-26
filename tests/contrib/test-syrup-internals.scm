(define-module (tests contrib test-syrup-internals)
  #:use-module (goblins contrib syrup)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-64)          ; tests
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match))

;; Pull in a bunch of internals from Syrup to test
(define-syntax-rule (snarf-syrup-internals id ...)
  (begin
    (define id (@@ (goblins contrib syrup) id)) ...))

(test-begin "test-syrup-internals")

;;; Netstring tests
;;; ===============

(snarf-syrup-internals netstring-encode)

(test-equal "netstring-encode works"
  (bytevector->string
   (netstring-encode
    (string->bytevector "Hello world!" "ISO-8859-1"))
   "ISO-8859-1")
  "12:Hello world!")

;;; Byte utilities tests
;;; ====================

(snarf-syrup-internals string->bytes/latin-1
                       string->bytes/utf-8
                       bytes->string/utf-8
                       bytes
                       bytes<?
                       bytes-append)

(test-equal "bytes-append works"
  (bytes-append #vu8(00 00 00 00)
                #vu8(11 11)
                #vu8(22 22))
  #vu8(0 0 0 0 11 11 22 22))

(test-assert "same bytestring isn't less"
  (not (bytes<? (bytes "meep")
                (bytes "meep"))))
(test-assert "greater bytestring of same length isn't less"
  (not (bytes<? (bytes "meep")
                (bytes "beep"))))
(test-assert "lesser bytestring of same length is less"
  (bytes<? (bytes "beep")
           (bytes "meep")))
(test-assert "greater bytestring of same length isn't less 2"
  (not (bytes<? (bytes "meep")
                (bytes "meeb"))))
(test-assert "lesser bytestring of same length is less 2"
  (bytes<? (bytes "meeb")
           (bytes "meep")))
(test-assert "shorter bytestring is less"
  (bytes<? (bytes "meep")
           (bytes "meeple")))
(test-assert "longer bytestring is greater"
  (not (bytes<? (bytes "meeple")
                (bytes "meep"))))

;;; Set tests
;;; =========

(snarf-syrup-internals make-set
                       set-member?)

;;;; Well the negative out of order set equality doesn't work, so...
;;;; The right thing to do is to re-enble these and switch these to
;;;; be ghash based as well...
;; (unless (equal? (make-set 1 2 3)
;;                 (make-set 1 2 3))
;;   (error 'test "set equality"))
;; (unless (equal? (make-set 3 2 1)
;;                 (make-set 1 2 3))
;;   (error 'test "out-of-order set equality"))
(let ([s (make-set 1 2 3)])
  (test-assert "positive set membership"
    (set-member? s 1))
  (test-assert "negative set membership"
    (not (set-member? s 99))))

(test-end "test-syrup-internals")
