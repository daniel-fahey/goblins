(define-module (tests contrib test-syrup)
  #:use-module (goblins contrib syrup)
  #:use-module (goblins abstract-types)
  #:use-module (goblins ghash)
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 iconv)
  #:use-module (srfi srfi-1)     ; lists
  #:use-module (srfi srfi-9)     ; records
  #:use-module (srfi srfi-64))   ; tests

(test-begin "test-syrup")

;; Pull in a bunch of internals from Syrup to test
(define-syntax-rule (snarf-syrup-internals id ...)
  (begin
    (define id (@@ (goblins contrib syrup) id)) ...))

(snarf-syrup-internals bytes
                       string->bytes/latin-1)

(test-equal "eof anywhere in a syrup-read is an eof"
  the-eof-object
  (call-with-input-bytevector (string->bytes/latin-1 "[3:foo")
                              syrup-read))

(define (alist->ghash alist)
  (hash-table->ghash (alist->hash-table alist)))

(define zoo-structure
  (make-tagged* (bytes "zoo")
                "The Grand Menagerie"
                (map alist->ghash
                     `(((species . ,(bytes "cat"))
                        (name . "Tabatha")
                        (age . 12)
                        (weight . 8.2)
                        (alive? . #t)
                        (eats . ,(make-gset (bytes "mice") (bytes "fish")
                                           (bytes "kibble"))))
                       ((species . ,(bytes "monkey"))
                        (name . "George")
                        (age . 6)
                        (weight . 17.24)
                        (alive? . #f)
                        (eats . ,(make-gset (bytes "bananas")
                                           (bytes "insects"))))
                       ((species . ,(bytes "ghost"))
                        (name . "Casper")
                        (age . -12)
                        (weight . -34.5)
                        (alive? . #f)
                        (eats . ,(make-gset)))))))

(define zoo-expected-bytes
  #vu8(60 51 58 122 111 111 49 57 34 84 104 101 32 71 114 97 110 100
       32 77 101 110 97 103 101 114 105 101 91 123 51 39 97 103 101
       49 50 43 52 39 101 97 116 115 35 52 58 102 105 115 104 52 58 109
       105 99 101 54 58 107 105 98 98 108 101 36 52 39 110 97 109 101 55
       34 84 97 98 97 116 104 97 54 39 97 108 105 118 101 63 116 54 39 119
       101 105 103 104 116 68 64 32 102 102 102 102 102 102 55 39 115 112
       101 99 105 101 115 51 58 99 97 116 125 123 51 39 97 103 101 54 43
       52 39 101 97 116 115 35 55 58 98 97 110 97 110 97 115 55 58 105 110
       115 101 99 116 115 36 52 39 110 97 109 101 54 34 71 101 111 114 103
       101 54 39 97 108 105 118 101 63 102 54 39 119 101 105 103 104 116 68
       64 49 61 112 163 215 10 61 55 39 115 112 101 99 105 101 115 54 58 109
       111 110 107 101 121 125 123 51 39 97 103 101 49 50 45 52 39 101 97
       116 115 35 36 52 39 110 97 109 101 54 34 67 97 115 112 101 114 54 39
       97 108 105 118 101 63 102 54 39 119 101 105 103 104 116 68 192 65 64
       0 0 0 0 0 55 39 115 112 101 99 105 101 115 53 58 103 104 111 115 116
       125 93 62))

(test-equal "Correctly encodes zoo structure"
  (syrup-encode zoo-structure)
  zoo-expected-bytes)

;; The extra encoding is a workaround for complexity around checking equality :P
(test-equal "Correctly decodes zoo structure"
  (syrup-encode (syrup-decode zoo-expected-bytes))
  (syrup-encode zoo-structure))

(test-equal "csexp backwards compat"
  (syrup-decode (bytes "(3:zoo (3:cat 7:tabatha))"))
  (list (bytes "zoo") (list (bytes "cat") (bytes "tabatha"))))

(define-record-type <foop>
  (make-foop blorp blap)
  foop?
  (blorp foop-blorp)
  (blap foop-blap))

(define (foop->record fb)
  (make-tagged* 'foop (foop-blorp fb) (foop-blap fb)))
  
(test-equal "marshaller works"
 (syrup-encode (list 'meep 'moop (make-foop 'fizzy 'water) 'bop)
               #:marshallers (list (cons foop? foop->record)))
 (bytes "[4'meep4'moop<4'foop5'fizzy5'water>3'bop]"))

(test-equal "unmarshaller works"
 (syrup-decode (bytes "[4'meep4'moop<4'foop5'fizzy5'water>3'bop]")
               #:unmarshallers (list (cons 'foop make-foop)))
 (list 'meep 'moop (make-foop 'fizzy 'water) 'bop))

(define-record-type <animal>
  (make-animal name noise)
  animal?
  (name animal-name)
  (noise animal-noise))
(define (serialize-animal label animal)
  (match animal
    [($ <animal> name noise)
     (make-tagged* label name noise)]))
(define-values (marshall::animal unmarshall::animal)
  (make-marshallers 'animal animal? make-animal serialize-animal))

(define-syrup-record-type <fruit>
  (make-fruit name color)
  fruit?
  fruit marshall::fruit unmarshall::fruit
  (name fruit-name)
  (color fruit-color))

(define marshallers
  (list marshall::animal
        marshall::fruit))
(define unmarshallers
  (list unmarshall::animal
        unmarshall::fruit))

(define cat (make-animal "Cat" 'meow))
(define banana (make-fruit "Banana" 'yellow))

(define friends
  `((cat-friend ,cat)
    (banana-friend ,banana)))
(define marshalled-friends
  (string->bytevector
   "[[10'cat-friend<6'animal3\"Cat4'meow>][13'banana-friend<5'fruit6\"Banana6'yellow>]]"
   "ISO-8859-1"))

(test-assert
    "Check that the can-marshall function works on its record type"
  ((car marshall::animal) cat))

(test-assert
    "Check that the can-marshall function returns false when given incorrect record type"
  (not ((car marshall::animal) banana)))

(define sticky-cat ((cdr marshall::animal) cat))
(test-assert
    "Check that marshalled cat returns syrup record"
  (tagged? sticky-cat))

(test-equal "Check that syrup-encode will marshall with marshallers correctly"
  marshalled-friends
  (syrup-encode friends
                #:marshallers marshallers))

(test-equal "Check that syrup-encode will unmarshall with unmarshallers correctly"
  friends
  (syrup-decode marshalled-friends
                #:unmarshallers unmarshallers))

(test-assert
    "Check the can-unmarshall function works for the correct label"
  ((car unmarshall::animal) (tagged-label sticky-cat)))

(define sticky-banana ((cdr marshall::fruit) banana))
(test-assert
    "Check that can-unmarshall returns false for the wrong label"
  (not ((car unmarshall::animal) (tagged-label sticky-banana))))

(test-equal "Check that unmarshalling returns correct data"
  cat
  (apply (cdr unmarshall::animal) (tagged-data sticky-cat)))

(test-end "test-syrup")
