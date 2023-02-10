;;; Copyright 2023 Vivi Langdon
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

(define-module (goblins actor-lib selfish-spawn)
  #:use-module (goblins)
  #:export (selfish-spawn))

(define (selfish-spawn constructor . args)

  (define (^selfish bcom)
    (lambda (self)
      (bcom (apply constructor bcom self args))))

  (let ((cons-name (procedure-property constructor 'name)))
    (set-procedure-property! ^selfish 'name cons-name))

  (let ((self (spawn ^selfish)))
    ;; now transition to the version with self
    ($ self self)
    self))

;; Racket test
;; (module+ test
;;   (require rackunit)
;;   (define am (make-actormap))
;;   (define (^narcissus bcom self stare-object)
;;     (lambda (how-i-feel)
;;       `(i-am ,self i-stare-into ,stare-object and-i-feel ,how-i-feel)))
;;   (define narcissus
;;     (actormap-run!
;;      am (lambda ()
;;           (selfish-spawn ^narcissus 'water))))
;;   (test-equal?
;;    "selfish-spawned actors know themselves"
;;    (actormap-peek am narcissus 'transfixed)
;;    `(i-am ,narcissus i-stare-into water and-i-feel transfixed)))
