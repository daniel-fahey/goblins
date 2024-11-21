;;; Copyright 2019-2023 Christine Lemmer-Webber
;;; Copyright 2023 David Thompson
;;; Copyright 2022-2024 Jessica Tallon
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


;; This module should largely be considered private and should typically
;; not be used directly. If you need these things, most of them are
;; exported from core and if they haven't been, likely it's because you
;; don't need them.
(define-module (goblins core-types)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (goblins utils define-applicable-record-type)
  #:export (<actormap>
            _make-actormap
            actormap?
            actormap-metatype
            actormap-data
            actormap-vat-connector
            actormap-aurie-counter
            actormap-ref
            actormap-set!
            set-actormap-aurie-counter!

            merge-actormap-aurie-counters!
            increment-actormap-aurie-counter!

            <actormap-metatype>
            make-actormap-metatype
            actormap-metatype?
            actormap-metatype-name
            actormap-metatype-ref-proc
            actormap-metatype-set!-proc
            actormap-metatype-for-each-proc

            <whactormap-data>
            make-whactormap-data
            whactormap-data?
            whactormap-data-wht

            whactormap?
            whactormap-ref
            whactormap-set!
            whactormap-metatype

            <transactormap-data>
            make-transactormap-data
            transactormap-data?
            transactormap-data-parent
            transactormap-data-delta
            transactormap-data-merged?
            set-transactormap-data-merged?!
            transactormap-merged?

            <local-object-refr>
            make-local-object-refr
            local-object-refr?
            local-object-refr-debug-name
            local-object-refr-vat-connector
            local-object-refr-aurie-id

            <local-promise-refr>
            make-local-promise-refr
            local-promise-refr?
            local-promise-refr-vat-connector

            <remote-object-refr>
            make-remote-object-refr
            remote-object-refr?
            remote-object-refr-captp-connector
            remote-object-refr-sealed-pos

            <remote-promise-refr>
            make-remote-promise-refr
            remote-promise-refr?
            remote-promise-refr-captp-connector
            remote-promise-refr-sealed-pos

            local-refr?
            local-refr-vat-connector
            remote-refr?
            remote-refr-captp-connector
            remote-refr-sealed-pos
            live-refr?
            promise-refr?

            <persistence-env>
            _make-persistence-env
            persistence-env?
            persistence-env-constructor->object-spec
            persistence-env-name->object-spec

            <portraitized-behavior>
            portraitize
            portraitized-behavior?
            portraitized-behavior-behavior
            portraitized-behavior-self-portrait

            <object-spec>
            make-object-spec
            object-spec?
            object-spec-name
            object-spec-constructor
            object-spec-rehydrator

            <versioned-data>
            versioned
            versioned-data?
            versioned-data-version
            versioned-data-data

            <redefinable-object>
            make-redefinable-object
            redefinable-object?
            redefinable-object-constructor
            set-redefinable-object-constructor!
            redefinable-object-rehydrator
            set-redefinable-object-rehydrator!

            <persistence-store>
            make-persistence-store
            persistence-store-read-proc
            persistence-store-save-proc

            make-persistable-object-identifier))

;; Actormaps, etc
;; ==============
(define-record-type <actormap>
  ;; TODO: This is confusing, naming-wise? (see make-actormap alias)
  (_make-actormap metatype data vat-connector aurie-counter)
  actormap?
  (metatype actormap-metatype)
  (data actormap-data)
  (vat-connector actormap-vat-connector)
  (aurie-counter actormap-aurie-counter set-actormap-aurie-counter!))

(define (merge-actormap-aurie-counters! old-actormap new-actormap)
  "Merge the NEW-ACTORMAP's counter onto OLD-ACTORMAP"
  (define old-actormap-aurie-counter (actormap-aurie-counter old-actormap))
  (define new-actormap-aurie-counter (actormap-aurie-counter new-actormap))
  (cond
   ((> old-actormap-aurie-counter new-actormap-aurie-counter)
    (error "Old actormap's counter is higher than new actormap's"))
   ((> new-actormap-aurie-counter old-actormap-aurie-counter)
    (set-actormap-aurie-counter! old-actormap new-actormap-aurie-counter))))

(define (increment-actormap-aurie-counter! actormap)
  "Increment ACTORMAP counter and return incremented number"
  (define new-ctr (1+ (actormap-aurie-counter actormap)))
  (set-actormap-aurie-counter! actormap new-ctr)
  new-ctr)

;; (set-record-type-printer!
;;  <actormap>
;;  (lambda (am port)
;;    (format port "#<actormap ~a>" (actormap-metatype-name (actormap-metatype am)))))

(define-record-type <actormap-metatype>
  (make-actormap-metatype name ref-proc set!-proc for-each-proc)
  actormap-metatype?
  (name actormap-metatype-name)
  (ref-proc actormap-metatype-ref-proc)
  (set!-proc actormap-metatype-set!-proc)
  (for-each-proc actormap-metatype-for-each-proc))

(define (actormap-set! am key val)
  ((actormap-metatype-set!-proc (actormap-metatype am))
   am key val)
  *unspecified*)

;; (-> actormap? local-refr? (or/c mactor? #f))
(define (actormap-ref am key)
  ((actormap-metatype-ref-proc (actormap-metatype am)) am key))

(define-record-type <whactormap-data>
  (make-whactormap-data wht)
  whactormap-data?
  (wht whactormap-data-wht))

(define (whactormap? obj)
  "Return #t if OBJ is a weak-hash actormap, else #f.

Type: Any -> Boolean"
  (and (actormap? obj)
       (eq? (actormap-metatype obj) whactormap-metatype)))

(define (whactormap-ref am key)
  (define wht (whactormap-data-wht (actormap-data am)))
  (hashq-ref wht key #f))

(define (whactormap-set! am key val)
  (define wht (whactormap-data-wht (actormap-data am)))
  (hashq-set! wht key val))

(define (whactormap-for-each proc am)
  (hash-for-each proc (whactormap-data-wht (actormap-data am))))

(define whactormap-metatype
  (make-actormap-metatype 'whactormap whactormap-ref whactormap-set!
                          whactormap-for-each))

;; Transactional actormaps
;; =======================

(define-record-type <transactormap-data>
  (make-transactormap-data parent delta merged?)
  transactormap-data?
  (parent transactormap-data-parent)
  (delta transactormap-data-delta)
  (merged? transactormap-data-merged? set-transactormap-data-merged?!))

(define (transactormap-merged? transactormap)
  (transactormap-data-merged? (actormap-data transactormap)))

;; Ref(r)s
;; =======

(define-record-type <local-object-refr>
  (make-local-object-refr debug-name vat-connector aurie-id)
  local-object-refr?
  (debug-name local-object-refr-debug-name)
  (vat-connector local-object-refr-vat-connector)
  (aurie-id local-object-refr-aurie-id))

(set-record-type-printer!
 <local-object-refr>
 (lambda (lor port)
   (match (local-object-refr-debug-name lor)
     [#f (display "#<local-object>" port)]
     [debug-name
      (format port "#<local-object ~a>" debug-name)])))

(define-record-type <local-promise-refr>
  (make-local-promise-refr vat-connector)
  local-promise-refr?
  (vat-connector local-promise-refr-vat-connector))

(set-record-type-printer!
 <local-promise-refr>
 (lambda (lpr port)
   (display "#<local-promise>" port)))

(define (local-refr? obj)
  "Return #t if OBJ is an object or promise reference in the current
process, else #f.

Type: Any -> Boolean"
  (or (local-object-refr? obj) (local-promise-refr? obj)))

(define (local-refr-vat-connector local-refr)
  (match local-refr
    [(? local-object-refr?)
     (local-object-refr-vat-connector local-refr)]
    [(? local-promise-refr?)
     (local-promise-refr-vat-connector local-refr)]))

;; Captp-connector should be a procedure which both sends a message
;; to the local node representative actor, but also has something
;; serialized that knows which specific remote node + session this
;; corresponds to (to look up the right captp session and forward)

(define-record-type <remote-object-refr>
  (make-remote-object-refr captp-connector sealed-pos)
  remote-object-refr?
  (captp-connector remote-object-refr-captp-connector)
  (sealed-pos remote-object-refr-sealed-pos))

(define-record-type <remote-promise-refr>
  (make-remote-promise-refr captp-connector sealed-pos)
  remote-promise-refr?
  (captp-connector remote-promise-refr-captp-connector)
  (sealed-pos remote-promise-refr-sealed-pos))

(define (promise-refr? maybe-promise)
  "Return #t if MAYBE-PROMISE is a promise reference, else #f.

Type: Any -> Boolean"
  (or (local-promise-refr? maybe-promise) (remote-promise-refr? maybe-promise)))

(define (remote-refr-captp-connector remote-refr)
  (match remote-refr
    [(? remote-object-refr?)
     (remote-object-refr-captp-connector remote-refr)]
    [(? remote-promise-refr?)
     (remote-promise-refr-captp-connector remote-refr)]))

(define (remote-refr-sealed-pos remote-refr)
  (match remote-refr
    [(? remote-object-refr?)
     (remote-object-refr-sealed-pos remote-refr)]
    [(? remote-promise-refr?)
     (remote-promise-refr-sealed-pos remote-refr)]))

(set-record-type-printer!
 <remote-object-refr>
 (lambda (lpr port)
   (display "#<remote-object>" port)))

(set-record-type-printer!
 <remote-promise-refr>
 (lambda (lpr port)
   (display "#<remote-promise>" port)))

(define (remote-refr? obj)
  "Return #t if OBJ is an object or promise reference in a different
process, else #f.

Type: Any -> Boolean"
  (or (remote-object-refr? obj)
      (remote-promise-refr? obj)))

(define (live-refr? obj)
  "Return #t if OBJ is a local or remote object or promise reference,
else #f.

Type: Any -> Boolean"
  (or (local-refr? obj)
      (remote-refr? obj)))

;; Persistence
;; ===========
(define-record-type <persistence-env>
  (_make-persistence-env constructor->object-spec name->object-spec)
  persistence-env?
  (constructor->object-spec persistence-env-constructor->object-spec)
  (name->object-spec persistence-env-name->object-spec))

;; Portraitized behavior
(define-record-type <portraitized-behavior>
  (portraitize beh self-portrait)
  portraitized-behavior?
  (beh portraitized-behavior-behavior)
  (self-portrait portraitized-behavior-self-portrait))

;; Used internally to represent each object in the persistent environment
(define-record-type <object-spec>
  (make-object-spec name constructor rehydrator)
  object-spec?
  (name object-spec-name)
  (constructor object-spec-constructor)
  (rehydrator _object-spec-rehydrator))

(define (object-spec-rehydrator obj-spec)
  (define cstr (object-spec-constructor obj-spec))
  (or (and (redefinable-object? cstr)
           (redefinable-object-rehydrator cstr))
      (_object-spec-rehydrator obj-spec)))

;; This while looking similar to the above this is used to specify versioned data
;; by objects in their self-portrait function. The `versioned' constructor is exported
;; which is used to created <version> + <portrait data> so the persistence system
;; can reliably detect when being given versioned data. This tagging is not exposed
;; anywhere else, including the resulting portraits.
(define-record-type <versioned-data>
  (versioned version data)
  versioned-data?
  (version versioned-data-version)
  (data versioned-data-data))

;; Used as a sort of "box" to restore objects to while keeping the actor
;; definition eq to itself when in persistence-envs
;; NOTE: this is an invocable/applicable struct so that we can call it.
(define-applicable-record-type <redefinable-object>
  (_make-redefinable-object procedure rehydrator)
  redefinable-object?
  (rehydrator redefinable-object-rehydrator set-redefinable-object-rehydrator!))

(define* (make-redefinable-object constructor #:optional rehydrator)
  "Construct a redefinable object for CONSTRUCTOR

Optionally, REHYDRATOR may be provided, which is a procedure for restoring
a persisted version of an object spawned via CONSTRUCTOR."
  (_make-redefinable-object constructor rehydrator))

(define redefinable-object-constructor
  applicable-record-procedure)

(define set-redefinable-object-constructor!
  set-applicable-record-procedure!)

(set-record-type-printer! <redefinable-object>
                          (lambda (ro op)
                            (format op "#<redefinable ~a>"
                                    (redefinable-object-constructor ro))))

;; Persistence stores
(define-record-type <persistence-store>
  (make-persistence-store read-proc save-proc)
  persistence-store?
  (read-proc persistence-store-read-proc)
  (save-proc persistence-store-save-proc))


;; This can be used as a stand in for when an actor needs to refer to a
;; object before it's been woken up... It's important we do *not* export
;; the accessors to the vat-id or object-id.
(define-record-type <persistable-object-identifier>
  (make-persistable-object-identifier vat-id object-id)
  persistable-object-identifier?
  (vat-id persistable-object-identifier-vat-id)
  (object-id persistable-object-identifier-object-id))
