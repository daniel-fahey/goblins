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

(define-module (tests actor-lib test-common)
  #:use-module (goblins core)
  #:use-module (goblins actor-lib common)
  #:use-module (srfi srfi-64))

(test-begin "test-common")

(define am (make-actormap))

(define s (actormap-spawn! am ^seteq 'a 'b 'c))
(test-equal #t (actormap-peek am s 'member? 'c))
(test-equal #f (actormap-peek am s 'member? 'd))
(actormap-poke! am s 'add 'd)
(test-equal #t (actormap-peek am s 'member? 'd))
(actormap-poke! am s 'remove 'c)
(test-equal #f (actormap-peek am s 'member? 'c))

(test-end "test-common")

