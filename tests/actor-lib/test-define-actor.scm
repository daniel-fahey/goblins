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
  #:use-module (goblins actor-lib define-actor)
  #:use-module ((goblins core-types)
                #:select (redefinable-object?))
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

;; Test an object which uses keys and optional values
(define-actor (^robot _bcom name #:optional color #:key [hp 100] ready?)
  (lambda ()
    (string-append
     "I am a "
     (if color
	 (format #f "~a robot" color)
	 "robot")
     (format #f " with ~a hit points left. " hp)
     (if ready?
	 "Lets rumble!"
	 "... not ready yet!"))))

(define am1 (make-actormap))
(define robot-env
  (make-persistence-env
   `((((tests utils test-define-actor) ^robot) ,^robot))))

(define smashtron500
  (actormap-spawn! am1 ^robot "Smashtron 5000" #:hp 200))
(define roadblock
  (actormap-spawn! am1 ^robot "Roadblock" 'red #:ready? #t))

(define-values (robot-portraits robot-roots)
  (actormap-take-portrait am1 robot-env smashtron500 roadblock))

(define restored-am1
  (make-actormap))
(define-values (restored-smashtron500 restored-roadblock)
  (actormap-restore restored-am1 robot-env robot-portraits robot-roots))

(test-equal
    "Check first restored robot has correct output"
  "I am a robot with 200 hit points left. ... not ready yet!"
  (actormap-peek restored-am1 restored-smashtron500))
(test-equal
    "Check second restored robot has correct output"
  "I am a red robot with 100 hit points left. Lets rumble!"
  (actormap-peek restored-am1 restored-roadblock))

;; This is a good sanity check and verifies define-actor without resturation.
(test-equal
    "Check first restored robot has same output as non-restored robot"
  (actormap-peek am1 smashtron500)
  (actormap-peek restored-am1 restored-smashtron500))
(test-equal
    "Check first restored robot has same output as non-restored robot"
  (actormap-peek am1 roadblock)
  (actormap-peek restored-am1 restored-roadblock))

(test-assert "By default, define-actor makes redefinable objects"
  (redefinable-object? ^cell))

(define-actor (^cell-frozen bcom value)
  #:frozen
  (case-lambda
    [() value]
    [(new-value) (bcom (^cell-frozen bcom new-value))]))

(test-assert "define-actor with #:frozen makes ordinary procedures"
  (and (not (redefinable-object? ^cell-frozen))
       (procedure? ^cell-frozen)))

(test-end "test-define-actor")
