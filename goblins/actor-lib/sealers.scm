;;; Copyright 2022 Jessica Tallon
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

(define-module (goblins actor-lib sealers)
  #:use-module (goblins)
  #:use-module ((goblins utils simple-sealers) #:prefix simple:)
  #:export (spawn-sealer-triplet))

(define* (spawn-sealer-triplet #:optional name
                               #:key (make-sealer-triplet simple:make-sealer-triplet))
  (define-values (seal unseal sealed?)
    (make-sealer-triplet name))

  (define (^sealed _bcom value)
    (define sealed-value (seal value))
    (lambda () sealed-value))

  (define (^sealer _bcom)
    (lambda (value)
      (spawn-named 'sealed-value ^sealed value)))

  (define (^unsealer _bcom)
    (define (unseal-it value)
      (unseal ($ value)))

    (lambda (sealed-value)
      (if (local-object-refr? sealed-value)
          (unseal-it sealed-value)
          (on sealed-value unseal-it #:promise? #t))))

  (define (^sealed? _bcom)
    (define (is-sealed? value)
      (and (local-object-refr? value)
           (sealed? ($ value))))

    (lambda (maybe-sealed)
      (if (promise-refr? maybe-sealed)
          (on maybe-sealed is-sealed? #:promise? #t)
          (is-sealed? maybe-sealed))))

  (values (spawn-named 'sealer ^sealer)
          (spawn-named 'unsealer ^unsealer)
          (spawn-named 'sealed? ^sealed?)))
