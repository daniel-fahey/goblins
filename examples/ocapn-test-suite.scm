;;; Copyright 2023-2024 Jessica Tallon
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(use-modules (goblins)
             (goblins vat)
             (goblins ocapn captp)
             (goblins ocapn ids)
             (goblins ocapn netlayer onion)
             (fibers conditions)
             (ice-9 iconv))

(define (trigger-gc)
  (define (^gc-collect _bcom)
    (lambda ()
      (sleep 1)
      (gc)))

  (define self (spawn ^gc-collect))
  (<-np self))

(define (^car _bcom color model)
  (lambda ()
    (format #f "Vroom! I am a ~a ~a car!" color model)))

(define (^car-factory _bcom)
  (lambda car-specs
    (define cars
      (map
       (lambda (car-spec)
               (apply spawn ^car car-spec))
       car-specs))
    (apply values cars)))

(define (^car-factory-builder _bcom)
  (lambda ()
    (spawn ^car-factory)))

(define (^echo _bcom)
  (lambda args
    (trigger-gc)
    args))

(define (^greeter _bcom)
  (lambda (target)
    (on (<- target "Hello")
        (lambda result
          (trigger-gc)))))

(define (^promise-resolver _bcom)
  (lambda ()
    (define-values (vow resolver)
      (spawn-promise-values))
    (list vow resolver)))

(define (^sturdyref-provider _bcom mycapn)
  (lambda (sref)
    (pk 'enlivening sref)
    ($ mycapn 'enliven sref)))

(define a-vat (spawn-vat))
(with-vat a-vat
  (define onion-netlayer
    (spawn ^onion-netlayer))
  (define mycapn (spawn-mycapn onion-netlayer))
  (define nonce-registry ($ mycapn 'get-registry))

  (define car-factory-builder (spawn ^car-factory-builder))
  (define car-factory-builder-swiss-num
    (string->bytevector "JadQ0++RzsD4M+40uLxTWVaVqM10DcBJ" "ascii"))
  ($ nonce-registry 'register car-factory-builder car-factory-builder-swiss-num)

  (define echo (spawn ^echo))
  (define echo-swiss-num
    (string->bytevector "IO58l1laTyhcrgDKbEzFOO32MDd6zE5w" "ascii"))
  ($ nonce-registry 'register echo echo-swiss-num)

  (define greeter (spawn ^greeter))
  (define greeter-swiss-num
    (string->bytevector "VMDDd1voKWarCe2GvgLbxbVFysNzRPzx" "ascii"))
  ($ nonce-registry 'register greeter greeter-swiss-num)

  (define promise-resolver (spawn ^promise-resolver))
  (define promise-resolver-swiss-num
    (string->bytevector "IokCxYmMj04nos2JN1TDoY1bT8dXh6Lr" "ascii"))
  ($ nonce-registry 'register promise-resolver promise-resolver-swiss-num)

  (define sturdyref-provider (spawn ^sturdyref-provider mycapn))
  (define sturdyref-provider-swiss-num
    (string->bytevector "gi02I1qghIwPiKGKleCQAOhpy3ZtYRpB" "ascii"))
  ($ nonce-registry 'register sturdyref-provider sturdyref-provider-swiss-num)

  (on (<- onion-netlayer 'our-location)
      (lambda (loc)
        (format #t "Connect test suite to: ~a\n" (ocapn-id->string loc)))))


(define forever (make-condition))
(wait forever)
