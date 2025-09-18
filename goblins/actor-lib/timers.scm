;;; Copyright 2025 Jessica Tallon
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
(define-module (goblins actor-lib timers)
  #:use-module (goblins vat)
  #:use-module (fibers timers)
  #:export (timeout))

(define* (timeout seconds #:optional [result #t])
  "Make a promise which will be fulfilled after @var{seconds}.

The returned promise will be fulfilled with @var{result}."
  ;; Note: This is assuming fibers based vats which might not be a great
  ;; assumption to be made for a general utility like a sleep procedure.
  ;; Maybe in the future we'd want to make this more general.
  (spawn-fibrous-vow (lambda () (sleep seconds) result)))
