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

(define-module (tests actor-lib test-pushdown)
  #:use-module (goblins)
  #:use-module (goblins actor-lib pushdown)
  #:use-module (srfi srfi-64))

(test-begin "test-pushdown")

(define am (make-actormap))

(define-values (pd-stack pd-forwarder)
  (actormap-run! am spawn-pushdown-pair))

(define first-actor
  (actormap-poke! am pd-stack 'spawn-push
                  (lambda (bcom prev foo)
                    (lambda (bar)
                      (list 'first prev foo bar)))
                  'foo))
(test-equal "Pushdown forwarder sends to first actor if only one on stack"
 (actormap-poke! am pd-forwarder 'bar)
 `(first #f foo bar))
(define second-actor
  (actormap-poke! am pd-stack 'spawn-push
                  (lambda (bcom prev foo)
                    (lambda (bar)
                      (list 'second prev foo bar)))
                  'foo2))
(test-equal "Pushdown forwarder sends to second actor once pushed onto stack"
 (actormap-poke! am pd-forwarder 'bar2)
 `(second ,first-actor foo2 bar2))

(actormap-poke! am pd-stack 'pop)

(test-equal "Pushdown forwarder sends to first actor again after first actor popped off"
 (actormap-poke! am pd-forwarder 'bar3)
 `(first #f foo bar3))

(define third-actor
  (actormap-spawn! am (lambda (bcom foo)
                        (lambda (bar)
                          (list 'third foo bar)))
                  'foo3))

(actormap-poke! am pd-stack 'push third-actor)

(test-equal "Pushdown stack 'push method works too"
  (actormap-poke! am pd-forwarder 'bar4)
  '(third foo3 bar4))

(test-end "test-pushdown")
