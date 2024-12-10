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
  (issue-deprecation-warning
   "Selfish-spawn is deprecated, use define-actor with the #:self keyword instead.")
  (define (^selfish bcom)
    (lambda (self)
      (bcom (apply constructor bcom self args))))

  (set-procedure-property! ^selfish 'name (procedure-name constructor))

  (let ((self (spawn ^selfish)))
    ;; now transition to the version with self
    ($ self self)
    self))
