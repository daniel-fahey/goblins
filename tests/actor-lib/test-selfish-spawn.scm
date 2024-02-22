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

(define-module (tests actor-lib test-selfish-spawn)
  #:use-module (goblins)
  #:use-module (goblins actor-lib selfish-spawn)
  #:use-module (srfi srfi-64))

(test-begin "test-selfish-spawn")

(define (^narcissus bcom self stare-object)
  (lambda (how-i-feel)
    `(i-am ,self i-stare-into ,stare-object and-i-feel ,how-i-feel)))

(define a-vat
  (spawn-vat))

(define narcissus
  (with-vat a-vat (selfish-spawn ^narcissus 'water)))

(test-equal "selfish-spawned actors know themselves"
  `(i-am ,narcissus i-stare-into water and-i-feel transfixed)
  (with-vat a-vat ($ narcissus 'transfixed)))

(test-end "test-selfish-spawn")
