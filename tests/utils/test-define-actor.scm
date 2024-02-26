;;; Copyright 2024 Jessica Tallon
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
;;;
(define-module (tests utils test-define-actor)
  #:use-module (goblins)
  #:use-module (goblins utils define-actor)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (fibers timers)
  #:use-module (srfi srfi-64))

(test-begin "test-define-actor")

(define-actor (^cell bcom value)
  (case-lambda
    [() value]
    [(new-value) (bcom (^cell bcom new-value))]))

(define am
  (make-actormap))

(define sword-cell
  (actormap-run!
   am
   (lambda ()
     (spawn ^cell 'sword))))

(define cell-env
  (make-persistence-env
   (list (list '((tests utils test-define-actor) ^cell) ^cell))))

(define-values (portraits roots)
  (actormap-take-portrait am cell-env sword-cell))

(define restored-am
  (make-actormap))

(define restored-sword-cell
  (actormap-restore restored-am cell-env portraits roots))

(test-equal "Got back the sword we put in from the sword cell"
  (actormap-peek restored-am restored-sword-cell)
  'sword)

(test-end "test-define-actor")
