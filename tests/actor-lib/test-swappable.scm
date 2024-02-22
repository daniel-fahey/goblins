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

(define-module (tests actor-lib test-swappable)
  #:use-module (goblins core)
  #:use-module (goblins actor-lib swappable)
  #:use-module (srfi srfi-64))

(test-begin "test-swappable")

(define am (make-actormap))

(define alice
  (actormap-spawn! am (lambda _ (lambda _ 'i-am-alice))))
(define bob
  (actormap-spawn! am (lambda _ (lambda _ 'i-am-bob))))

(define-values (proxy-friend swap)
  (actormap-run!
   am
   (lambda ()
     (swappable alice))))

(test-equal "swappable proxy defaults to first entity"
 'i-am-alice
 (actormap-peek am proxy-friend))

(actormap-run! am (lambda () (swap bob)))

(test-equal "swappable proxy swaps"
 'i-am-bob
 (actormap-peek am proxy-friend))

(test-end "test-swappable")
