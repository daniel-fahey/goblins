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

(define ro-a-cell
  (actormap-run!
   am
   (lambda ()
     (cell->read-only a-cell))))
(test-eq
    "Read from a read-only cell"
  (actormap-peek am ro-a-cell)
  'foo)
(test-error
 "Cannot write to a read-only cell"
 #t
 (actormap-poke! am ro-a-cell 'foobar))

(define wo-a-cell
  (actormap-run!
   am
   (lambda ()
     (cell->write-only a-cell))))
(actormap-poke! am wo-a-cell 'baz)
(test-eq
    "Can write to a write-only cell"
  'baz
  (actormap-peek am a-cell))
(test-error
 "Cannot read from a write-only cell"
 #t
 (actormap-peek am wo-a-cell))

(test-end "test-cell")

