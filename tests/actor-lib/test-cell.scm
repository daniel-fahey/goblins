;;; Copyright 2020-2021 Christine Lemmer-Webber
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

(define-module (tests actor-lib test-ward)
  #:use-module (goblins core)
  #:use-module (goblins actor-lib cell)
  #:use-module (srfi srfi-64))

(test-begin "test-cell")

(define am (make-whactormap))

(define a-cell
  (actormap-spawn! am ^cell))
(test-eq
 "cell without default value and unset is #f"
 (actormap-peek am a-cell)
 #f)
(actormap-poke! am a-cell 'foo)
(test-eq
 "cell after being set retains value"
 (actormap-peek am a-cell)
 'foo)

(test-eq
 "cell default values"
 (actormap-peek am (actormap-spawn! am ^cell 'hello))
 'hello)

(test-end "test-cell")

