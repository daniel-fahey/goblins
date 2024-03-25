;;; Copyright 2020-2022 Christine Lemmer-Webber
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

(define-module (tests actor-lib test-facet)
  #:use-module (goblins core)
  #:use-module (goblins define-actor)
  #:use-module (goblins actor-lib facet)
  #:use-module (tests utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(test-begin "test-facet")

(define am (make-whactormap))

(define-actor (^wizard bcom)
  (match-lambda*
    [('magic-missile level)
     (format #f "Casts magic missile level ~a!"
             level)]
    [('flame-tongue level)
     (format #f "Casts flame tongue level ~a!"
             level)]
    [('world-ender level)
     (format #f "Casts world ender level ~a!"
             level)]))
(define all-powerful-wizard
  (actormap-spawn! am ^wizard))
(define faceted-wizard
  (actormap-spawn! am ^facet all-powerful-wizard
                   'magic-missile
                   'flame-tongue))

(test-equal
 "Casts magic missile level 2!"
 (actormap-peek am faceted-wizard 'magic-missile 2))
(test-equal
 "Casts flame tongue level 3!"
 (actormap-peek am faceted-wizard 'flame-tongue 3))
(test-error
 (actormap-peek am faceted-wizard 'world-ender 99))

;; Persistence
(define env
  (make-persistence-env
   `((((tests actor-lib test-facet) ^wizard) ,^wizard))
   #:extends facet-env))
(define-values (am* faceted-wizard*)
  (persist-and-restore am env faceted-wizard))
(test-equal "Can cast spell after rehydration though facet"
  "Casts magic missile level 50!"
  (actormap-peek am* faceted-wizard* 'magic-missile 50))
(test-error
 "Cannot cast non-faceted method after rehydration"
 #t
 (actormap-peek am* faceted-wizard* 'world-ender 50))

(test-end "test-facet")

