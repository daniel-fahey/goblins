;;; Copyright 2019-2023 Christine Lemmer-Webber
;;; Copyright 2023 Juliana Sims
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

(define-module (goblins actor-lib opportunistic)
  #:use-module (goblins)
  #:export (select-$/<-
            run-$/<-))

;; A helper to select $ or <-.
;; Combined they look like a cartoon character swearing.
;; Sometimes we jokingly call these "cartoon swears."
;; But really this is "opportunistically using $ or otherwise use <-"
(define (select-$/<- to-refr)
  "Select $ opportunistically if to-refr is a near object, otherwise use <-.

Type: Actor -> (U $ <-)"
  (if (and (local-object-refr? to-refr)
           (near-refr? to-refr))
      $ <-))

(define (run-$/<- to-refr . args)
  "Invoke to-refr with $ opportunistically if a near object, otherwise use <-,
passing ARGS.

Type: Actor Any ... -> Any"
  (apply (select-$/<- to-refr) to-refr args))
