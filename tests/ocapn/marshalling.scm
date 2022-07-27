(define-module (tests ocapn marshalling)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 iconv)
  #:use-module (goblins ocapn marshalling)
  #:use-module (goblins contrib syrup))

(test-begin "marshalling")

(define-record-type <animal>
  (make-animal name noise)
  animal?
  (name animal-name)
  (noise animal-noise))
(define-values (marshall::animal unmarshall::animal)
  (make-marshallers <animal> #:name 'animal))

(define-record-type <fruit>
  (make-fruit name color)
  fruit?
  (name fruit-name)
  (color fruit-color))
(define-values (marshall::fruit unmarshall::fruit)
  (make-marshallers <fruit> #:name 'fruit))

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
  (syrec? sticky-cat))

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
  ((car unmarshall::animal) (syrec-label sticky-cat)))

(define sticky-banana ((cdr marshall::fruit) banana))
(test-assert
    "Check that can-unmarshall returns false for the wrong label"
  (not ((car unmarshall::animal) (syrec-label sticky-banana))))

(test-equal
    "Check that unmarshalling returns correct data"
  (apply (cdr unmarshall::animal) (syrec-args sticky-cat))
  cat)

(test-end "marshalling")
