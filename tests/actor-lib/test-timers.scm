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

(define-module (tests actor-lib test-timers)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib timers)
  #:use-module (tests utils)
  #:use-module (srfi srfi-64))

(test-begin "test-timers")

(define vat (spawn-vat))

(test-equal "After rehydration read-only cell can still be read"
  #(ok hello)
  (resolve-vow-and-return-result
   vat
   (lambda ()
     (timeout 1 'hello))))

(test-end "test-timers")
