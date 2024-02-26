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

(define-module (tests actor-lib test-opportunistic)
  #:use-module (goblins)
  #:use-module (goblins actor-lib opportunistic)
  #:use-module (ice-9 curried-definitions)
  #:use-module (srfi srfi-64))

(test-begin "test-opportunistic")

(define vat-a (spawn-vat))
(define vat-b (spawn-vat))
(define ((^simple-robot bcom))
  'beep-boop)
(define robot-a
  (with-vat vat-a
    (spawn ^simple-robot)))
(define robot-b
  (with-vat vat-b
    (spawn ^simple-robot)))

(define ((^swear-selector bcom) to)
  (select-$/<- to))

(define swear-selector-a
  (with-vat vat-a
    (spawn ^swear-selector)))

(test-eq "select-$/<- to object on same vat gets $"
 $
 (with-vat vat-a
   ($ swear-selector-a robot-a)))

(test-eq "select-$/<- to object on remote vat gets <-"
 <-
 (with-vat vat-a
   ($ swear-selector-a robot-b)))

(test-end "test-opportunistic")
