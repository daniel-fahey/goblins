;;; Copyright 2019-2023 Christine Lemmer-Webber
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

(define-module (goblins actor-lib select-swear)
  #:use-module (goblins)
  #:export (select-$/<-
            run-$/<-))

;; A helper to select $ or <-.
;; Combined they look like a cartoon character swearing.
(define (select-$/<- to-refr)
  (if (and (local-object-refr? to-refr)
           (near-refr? to-refr))
      $ <-))

(define (run-$/<- to-refr . args)
  (apply (select-$/<- to-refr) to-refr args))
