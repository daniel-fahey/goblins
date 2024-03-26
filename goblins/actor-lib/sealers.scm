;;; Copyright 2020-2021 Christine Lemmer-Webber
;;; Copyright 2022 Jessica Tallon
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

(define-module (goblins actor-lib sealers)
  #:use-module (goblins core)
  #:use-module (goblins define-actor)
  #:use-module (goblins actor-lib methods)
  #:use-module ((goblins utils simple-sealers) #:prefix simple:)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (spawn-sealer-triplet
	    make-spawn-sealer-triplet
	    default-sealers-alist
	    sealers-env))

;; When persisting sealers, we need to make sure they persist and
;; restore with the same sealer triplet maker. To do this we need to
;; pass in a function which includes all the ids and mappings to the
;; sealer triplet maker possible in the object graph.
(define default-sealers-alist
  `((((goblins utils simple-sealers) simple) . ,simple:make-sealer-triplet)))

(define (alist-switch-direction alist)
  "Converts alist so the value is the key and the key is the value"
  (map
   (lambda (cons-cell)
     (cons (cdr cons-cell) (car cons-cell)))
   alist))

(define* (make-spawn-sealer-triplet namespace known-sealers
				    #:optional default-make-sealer-triplet)
  "Make a spawn-sealer-triplet for a given set of known sealers

The actor-lib sealer system is built to be flexible to allow for
different sealers to be used. Sealers are not persistable so in order
to make sealers which are there needs to be a set of known sealers
which are named. The known sealers are an alist of:

(unique name . spawn-sealer-triplet)

When persisted the unique name is serialized and upon resturation it
is used.

The namespace argument should be a symbol which is unique to this
spawn-sealer-triplet. This namespace is used in the returned
persistence environment to ensure sealers are persisted and restored
belonging to this specific triplet spawner.

Optionally a default-make-sealer-triplet can be provided which will be
the default when spawning a new sealer triplet with the
spawn-sealer-triplet function returned if no specific one is
provided."
  
  (define known-sealer-triplet-ids
    (alist-switch-direction known-sealers))

  (define-actor (^sealer-triplet _bcom sealer-id #:optional name)
    #:frozen
    (define make-sealer-triplet
      (assoc-ref known-sealers sealer-id))
    (define-values (seal unseal sealed?)
      (make-sealer-triplet name))

    (methods
     [seal seal]
     [unseal unseal]
     [sealed? sealed?]))

  (define-actor (^sealed _bcom triplet value)
    #:frozen
    (lambda ()
      ($ triplet 'seal value)))

  (define-actor (^sealer _bcom triplet)
    #:frozen
    (lambda (value)
      (spawn-named 'sealed-value ^sealed triplet value)))

  (define-actor (^unsealer _bcom triplet)
    #:frozen
    (define (unseal-it value)
      ($ triplet 'unseal ($ value)))
    
    (lambda (sealed-value)
      (if (local-object-refr? sealed-value)
	  (unseal-it sealed-value)
	  (on sealed-value unseal-it #:promise? #t))))

  (define-actor (^sealed? _bcom triplet)
    #:frozen
    (define (is-sealed? value)
      (and (local-object-refr? value)
           ($ triplet 'sealed? ($ value))))

    (lambda (maybe-sealed)
      (if (promise-refr? maybe-sealed)
	  (on maybe-sealed is-sealed? #:promise? #t)
	  (is-sealed? maybe-sealed))))
  
  (define* (spawn-sealer-triplet #:optional name
				 #:key (make-sealer-triplet default-make-sealer-triplet))
    "Return seal, unseal, and check capabilities.

The optional NAME argument is the name of the record type when printed by the
Guile display, write, etc. procedures. The optional keyword MAKE-SEALER-TRIPLET
argument is a procedure creating a seal record and setting its NAME as
described.

Type: (Optional (U String Symbol))
(Optional (#:make-sealer-triplet
            ((Optional (U String Symbol)) -> (Values Sealer Unsealer Checker))))
-> (Values Sealer Unsealer Checker)"
    (define make-sealer-triplet-id
      (assq-ref known-sealer-triplet-ids make-sealer-triplet))
    (define sealer-triplet
      (spawn ^sealer-triplet make-sealer-triplet-id name))

    (values (spawn-named 'sealer ^sealer sealer-triplet)
            (spawn-named 'unsealer ^unsealer sealer-triplet)
	    (spawn-named 'sealed? ^sealed? sealer-triplet)))
  
  (define sealers-env
    (make-persistence-env
     `(((,namespace ^sealer-triplet) ,^sealer-triplet)
       ((,namespace ^sealed) ,^sealed)
       ((,namespace ^sealer) ,^sealer)
       ((,namespace ^unsealer) ,^unsealer)
       ((,namespace ^sealed?) ,^sealed?))))

  (values spawn-sealer-triplet sealers-env))

(define-values (spawn-sealer-triplet sealers-env)
  (make-spawn-sealer-triplet
   '(goblins actor-lib sealers)
   default-sealers-alist
   simple:make-sealer-triplet))
