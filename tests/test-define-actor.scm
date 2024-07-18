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
(define-module (tests test-define-actor)
  #:use-module (goblins core)
  #:use-module (goblins define-actor)
  #:use-module ((goblins core-types)
                #:select (redefinable-object?))
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (fibers timers)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1)
                #:select (first second third))
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
  (actormap-restore! restored-am cell-env portraits roots))

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
  (actormap-restore! restored-am1 robot-env robot-portraits robot-roots))

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

(define am2 (make-actormap))

;; #:version without #:portrait
(define-actor (^cell-versioned bcom value)
  #:version (+ 40 2)
  (case-lambda
    [() value]
    [(new-value) (bcom (^cell-versioned bcom new-value))]))

(define versioned-env
  (make-persistence-env
   `((((tests utils test-define-actor) ^cell-versioned) ,^cell-versioned))))

(define versioned-cell
  (actormap-spawn! am2 ^cell-versioned 'meep))

(define-values (versioned-portraits versioned-roots)
  (actormap-take-portrait am2 versioned-env versioned-cell))

(define version
  (match (hash-ref versioned-portraits (car versioned-roots))
    ((_name _debug-name version _data) version)))

(test-eqv "#:version for define-actor works" 42 version)

(define am3 (make-actormap))

;; This is an observation from Safe Serialization under Mutual Suspicion by
;; Mark Miller... an object can become aware of how many times it has been
;; restored.  In this case, we do so by studying how many times we've been
;; persisted by tagging that information
(define-actor (^cell-resurrection-aware bcom #:optional val)
  #:portrait (lambda () (list (list 'persisted val)))
  (case-lambda
    (() val)
    ((new-val) (bcom (^cell-resurrection-aware new-val)))))

(define resurrection-env
  (make-persistence-env
   `((((tests utils test-define-actor) ^cell-resurrection-aware)
      ,^cell-resurrection-aware))))

(define resurrection-cell
  (actormap-spawn! am3 ^cell-resurrection-aware 'meep))

;; Let's do two manual persists
(define restored-am3 (make-actormap))
(define restored-restored-am3 (make-actormap))

(define-values (resurrected-portraits resurrected-roots)
  (actormap-take-portrait am3 resurrection-env resurrection-cell))

(define-values (restored-rc)
  (actormap-restore! restored-am3 resurrection-env
                    resurrected-portraits resurrected-roots))

(test-equal '(persisted meep)
  (actormap-peek restored-am3 restored-rc))

(define-values (resurrected2x-portraits resurrected2x-roots)
  (actormap-take-portrait restored-am3 resurrection-env restored-rc))

(define-values (restored2x-rc)
  (actormap-restore! restored-restored-am3 resurrection-env
                    resurrected2x-portraits resurrected2x-roots))

(test-equal '(persisted (persisted meep))
  (actormap-peek restored-restored-am3 restored2x-rc))


(define am4 (make-actormap))

;; #:version with #:portrait
(define-actor (^cell-portrait-version bcom #:optional val)
  #:portrait (lambda ()
               (list (list 'persisted2 val)))
  #:version 2
  (case-lambda
    (() val)
    ((new-val) (bcom (^cell-portrait-version new-val)))))

;; #:version and #:portrait both specified but they match
(define-actor (^cell-portrait-version-match bcom #:optional val)
  #:portrait (lambda ()
               (versioned 'two (list (list 'persisted2-match val))))
  #:version 'two
  (case-lambda
    (() val)
    ((new-val) (bcom (^cell-portrait-version-match new-val)))))

(define-actor (^cell-portrait-version-mismatch bcom #:optional val)
  #:portrait (lambda ()
               (versioned 67 (list (list 'persisted2-mismatch val))))
  #:version 22
  (case-lambda
    (() val)
    ((new-val) (bcom (^cell-portrait-version-mismatch new-val)))))

(define portrait-version-env
  (make-persistence-env
   `((((tests utils test-define-actor) ^cell-portrait-version)
      ,^cell-portrait-version)
     (((tests utils test-define-actor) ^cell-portrait-version-match)
      ,^cell-portrait-version-match)
     (((tests utils test-define-actor) ^cell-portrait-version-mismatch)
      ,^cell-portrait-version-mismatch))))

(define cpv
  (actormap-spawn! am4 ^cell-portrait-version 'bloop))
(define cpv-match
  (actormap-spawn! am4 ^cell-portrait-version-match 'blop))
(define cpv-mismatch
  (actormap-spawn! am4 ^cell-portrait-version-mismatch 'blech))

(define-values (portrait-version-portraits portrait-version-roots)
  (actormap-take-portrait am4 portrait-version-env cpv cpv-match))

(define-values (cpv-version cpv-data)
  (match (hash-ref portrait-version-portraits (first portrait-version-roots))
    ((_name _debug-name version data)
     (values version data))))
(define-values (cpv-match-version cpv-match-data)
  (match (hash-ref portrait-version-portraits (second portrait-version-roots))
    ((_name _debug-name version data)
     (values version data))))

(test-equal "#:portrait and #:version compose, version"
  2 cpv-version)
(test-equal "#:portrait and #:version compose, data"
  '((persisted2 bloop)) cpv-data)

(test-equal "#:portrait and #:version compose when both providing version and matching, version"
  'two cpv-match-version)
(test-equal "#:portrait and #:version compose when both providing version and matching, data"
  '((persisted2-match blop)) cpv-match-data)

(test-error "Error raised when #:portrait provides version mismatching with #:version"
            (actormap-take-portrait am4 portrait-version-env cpv-mismatch))


;; Testing #:restore

(define-actor (^cell-restore bcom #:optional val)
  #:restore (lambda (version val)
              (spawn ^cell-restore (list 'restored version val)))
  #:version 2
  (case-lambda
    (() val)
    ((new-val) (bcom (^cell-restore new-val)))))


(define restorable-env
  (make-persistence-env
   `((((tests utils test-define-actor) ^cell-restore)
      ,^cell-restore))))

(define am5 (make-actormap))
(define restored-am5 (make-actormap))

(define cr
  (actormap-spawn! am5 ^cell-restore 'foop))

(define-values (restorable-portraits restorable-roots)
  (actormap-take-portrait am5 restorable-env cr))

(define-values (restored-cr)
  (actormap-restore! restored-am5 restorable-env
                     restorable-portraits restorable-roots))

(test-equal "restore procedure works"
  '(restored 2 foop)
  (actormap-peek restored-am5 restored-cr))

(test-end "test-define-actor")
