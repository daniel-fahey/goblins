;; Copyright (C) 2022 Jessica Tallon

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
(define-module (goblins ocapn marshalling)
  #:use-module (goblins abstract-types)
  #:use-module (goblins contrib syrup)
  #:export (make-marshallers))

;; This produces syrup serialisation and deserialisation for a record
;; type with the option of a custom syrup record label. This will
;; return two values:
;; 1) marshallers: cons cell (<predicate?> <serialiser>)
;; 2) unmarshaller: cons cell (<predicate?> <unserialiser>)
(define* (make-marshallers record #:key [name #f])
  (define syrup-label
    (or name (record-type-name record)))
  (define our-record?
    (record-predicate record))
  (define make-record
    (record-type-constructor record))

  (define field-accessors
    (map (lambda (field) (record-accessor record field))
	 (record-type-fields record)))

  (define marshaller
    (cons
     (lambda (obj)
       (our-record? obj))
     (lambda (obj)
       (apply
	make-tagged*
	syrup-label
	(map (lambda (get-field) (get-field obj))
	     field-accessors)))))
  (define unmarshaller
    (cons
     (lambda (label)
       (eq? label syrup-label))
     make-record))

  (values marshaller
	  unmarshaller))
