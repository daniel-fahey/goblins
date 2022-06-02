(define-module (tests ocapn marshalling)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-9)
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

(define cat (make-animal "Cat" 'meow))
(define banana (make-fruit "Banana" 'yellow))

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

(test-assert
    "Check the can-unmarshall function works on its own marshalled data"
  ((car unmarshall::animal) sticky-cat))

(define sticky-banana ((cdr marshall::fruit) banana))
(test-assert
    "Check that can-unmarshall returns false when given other data"
  (not ((car unmarshall::animal) sticky-banana)))

(test-equal
    "Check that unmarshalling returns correct data"
  ((cdr unmarshall::animal) sticky-cat)
  cat)

(test-end "marshalling")
