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

            <mactor:object>
            make-mactor:object
            mactor:object?
            mactor:object-behavior
            mactor:object-constructor-refr
            mactor:object-spawned-constructor
            mactor:object-self-portrait
            mactor:object-become-unsealer
            mactor:object-become?

            <m~eventual>
            make-m~eventual
            m~eventual?
            m~eventual-resolver-unsealer
            m~eventual-resolver-tm?

            <m~unresolved>
            make-m~unresolved
            m~unresolved?
            m~unresolved-eventual
            m~unresolved-listeners

            <mactor:naive>
            make-mactor:naive
            mactor:naive?
            mactor:naive
            mactor:naive-unresolved
            mactor:naive-waiting-messages

            <mactor:question>
            make-mactor:question
            mactor:question?
            mactor:question-unresolved
            mactor:question-captp-connector
            mactor:question-question-finder

            <mactor:closer>
            make-mactor:closer
            mactor:closer?
            mactor:closer-unresolved
            mactor:closer-point-to
            mactor:closer-history
            mactor:closer-waiting-messages

            <mactor:remote-link>
            make-mactor:remote-link
            mactor:remote-link?
            mactor:remote-link-eventual
            mactor:remote-link-point-to

            <mactor-local-link>
            make-mactor:local-link
            mactor:local-link?
            mactor:local-link-point-to

            <mactor-aurie-local-link>
            make-mactor:aurie-local-link
            mactor:aurie-local-link?
            mactor:aurie-local-link-point-to
            mactor:aurie-local-link-depiction

            <mactor:encased>
            make-mactor:encased
            mactor:encased?
            mactor:encased-val

            <mactor:broken>
            make-mactor:broken
            mactor:broken?
            mactor:broken-problem


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

            make-persistable-object-identifier
            persistable-object-identifier?
            persistable-object-identifier-vat-id
            persistable-object-identifier-object-id))

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
;; to the local peer representative actor, but also has something
;; serialized that knows which specific remote peer + session this
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


;; Mactors
;; =======

;;;                    .======================.
;;;                    | The World of Mactors |
;;;                    '======================'
;;;
;;; This is getting really deep into the weeds and is really only
;;; relevant to anyone hacking on this module.
;;;
;;; Mactors are only ever relevant to the internals of a vat, but they
;;; do define some common behaviors.
;;;
;;; Here are the categories and transition states:
;;;
;;;         Unresolved                      Resolved
;;;  ____________________________  ___________________________
;;; |                            ||                           |
;;;
;;; [aurie-local-link] .------------------->.        [object]
;;;                    |                    |
;;;                    |    .--.            |    .-->[local-link]
;;;     [naive]----->. |    v  |            |    |
;;;                  +>+->[closer]--------->'--->+-->[encased]
;;;    [question]--->' |       |                 |
;;;                    |       |                 '-->[broken]
;;;                    '------>'--->[remote-link]      ^
;;;                                     |              |
;;;                                     '------------->'
;;;
;;; |__________________________________________||_____________|
;;;                    Eventual                     Settled
;;;
;;; The four major categories of mactors:
;;;
;;;  - Unresolved: A promise that has never been fulfilled or broken.
;;;  - Resolved: Either an object with its own handler or a promise which
;;;    has been fulfilled to some value/object reference or which has broken.
;;;
;;; and:
;;;
;;;  - Eventual: Something which *might* eventually transition its state.
;;;  - Settled: Something which will never transition its state again.
;;;
;;; The surprising thing here is that there is any distinction between
;;; unresolved/resolved and eventual/settled at all.  The key to
;;; understanding the difference is observing that a mactor:remote-link
;;; might become broken upon network disconnect from that object.
;;;
;;; One intersting observation is that if you have a local-object-refr that
;;; it is sure to correspond to a mactor:object.  A local-promise-refr can
;;; correspond to any object state *except* for mactor:object (if a promise
;;; resolves to a local object, it must point to it via mactor:local-link.)
;;; (remote-refrs of course never correspond to a mactor on this peer;
;;; those are managed by captp.)
;;;
;;; The mactor:aurie-local-link is a special type of symlink which is used
;;; during restoration. It's generally used to point at unresolved promises, it
;;; includes a special slot to include additional info to help persist it.
;;; Currently, Aurie persists unresolved promises as broken, which means far
;;; refrs which are resolved initially as unresolved promises, would have become
;;; broken. This isn't what we want so we can include the underlying portrait
;;; data in this mactor which Aurie can then use later when persisting this.
;;;
;;; See also:
;;;  - The comments above each of these below
;;;  - "Miranda methods":
;;;      http://www.erights.org/elang/blocks/miranda.html
;;;  - "Reference mechanics":
;;;      http://erights.org/elib/concurrency/refmech.html

;; local-objects are the most common type, have a message handler
;; which specifies how to respond to the next message, as well as
;; a predicate and unsealer to identify and unpack when a message
;; handler specifies that this actor would like to "become" a new
;; version of itself (get a new handler)
(define-record-type <mactor:object>
  (make-mactor:object behavior constructor-refr spawned-constructor
                      self-portrait become-unsealer become?)
  mactor:object?
  ;; Behavior procedure
  (behavior mactor:object-behavior)
  ;; Reference to the constructor procedure or redefinable-object-constructor
  ;; this actor was spawned from
  ;; TODO: rename this, it's not a live-refr, and it kind of sounds like it is
  (constructor-refr mactor:object-constructor-refr)
  ;; This is the inner constructor *procedure*, which is unboxed from a
  ;; redefinable-object-constructor, so we can compare if the constructor
  ;; changed when doing an `actormap-replace-behavior'
  (spawned-constructor mactor:object-spawned-constructor)
  ;; The object's self-portrait procedure, if it exists
  (self-portrait mactor:object-self-portrait)
  ;; The following two are the predicate and unsealer from a
  ;; `make-become-sealer-triplet', specific to this actor
  (become-unsealer mactor:object-become-unsealer)
  (become? mactor:object-become?))

;; The other kinds of mactors correspond to promises and their resolutions.

;; There are two supertypes here which are not used directly:
;; mactor:unresolved and mactor:eventual.  See above for an explaination
;; of what these mean.
;; These are never directly exposed as mactors, hence the ~
(define-record-type <m~eventual>
  (make-m~eventual resolver-unsealer resolver-tm?)
  m~eventual?
  ;; We can still be resolved, so identify who is allowed to do that
  ;; and provide a mechanism for unsealing the resolution
  (resolver-unsealer m~eventual-resolver-unsealer)
  (resolver-tm? m~eventual-resolver-tm?))
(define-record-type <m~unresolved>
  (make-m~unresolved eventual listeners)
  m~unresolved?
  ;; the <m~eventual> info
  (eventual m~unresolved-eventual)
  ;; Who's listening for a resolution?
  (listeners m~unresolved-listeners))

;; The most common kind of freshly made promise is a naive one.
;; It knows no interesting information about how it will eventually
;; become what it will.
;; Since it knows of no closer information it keeps a queue of waiting
;; messages which will eventually be transmitted.
(define-record-type <mactor:naive>
  (make-mactor:naive unresolved waiting-messages)
  mactor:naive?
  (unresolved mactor:naive-unresolved)
  ;; All of these get "rewritten" as this promise is either resolved
   ;; or moved closer to resolution.
  (waiting-messages mactor:naive-waiting-messages))

;; A special kind of "freshly made" promise which also corresponds to being
;; a question on the remote end.  Keeps track of the captp-connector
;; relevant to this connection so it can send it messages and the
;; question-finder that it corresponds to (used for passing along messages).
(define-record-type <mactor:question>
  (make-mactor:question unresolved captp-connector question-finder)
  mactor:question?
  (unresolved mactor:question-unresolved)
  (captp-connector mactor:question-captp-connector)
  (question-finder mactor:question-question-finder))

;; "You make me closer to God" -- Nine Inch Nails
;; Well, in this case we're actually just "closer to resolution"...
;; pointing at some other promise that isn't us.
;;
;; NOTE: Any attempt to remove this in favor of "deferring an answer
;; until fulfillment is possible" should think through whether it will
;; also prevent cycles.  A great deal of work went into that here.
(define-record-type <mactor:closer>
  (make-mactor:closer unresolved point-to history waiting-messages)
  mactor:closer?
  (unresolved mactor:closer-unresolved)
  ;; Who do we currently point to?
  (point-to mactor:closer-point-to)
  ;; A set of promises we used to point to before they themselves
  ;; resolved... used to detect cycles
  (history mactor:closer-history)
  ;; Any messages that are waiting to be passed along...
  ;; Currently only if we're pointing to a remote-promise, otherwise
  ;; this will be an empty list.
  (waiting-messages mactor:closer-waiting-messages))

;; Point at a remote object.
;; It's eventual because, well, it could still break on network partition.
(define-record-type <mactor:remote-link>
  (make-mactor:remote-link eventual point-to)
  mactor:remote-link?
  (eventual mactor:remote-link-eventual)
  (point-to mactor:remote-link-point-to))

;; Link to an object on the same peer.
(define-record-type <mactor:local-link>
  (make-mactor:local-link point-to)
  mactor:local-link?
  (point-to mactor:local-link-point-to))

;; Special local link type used by aurie. It includes a field (depiction) which
;; can hold additional info to help aurie persist it correctly. See the world
;; of mactors comment for a better explanation.
(define-record-type <mactor:aurie-local-link>
  (make-mactor:aurie-local-link point-to depiction)
  mactor:aurie-local-link?
  (point-to mactor:aurie-local-link-point-to)
  (depiction mactor:aurie-local-link-depiction))

;; A promise that has resolved to some value
(define-record-type <mactor:encased>
  (make-mactor:encased val)
  mactor:encased?
  (val mactor:encased-val))

;; Breakage (and remember why!)
(define-record-type <mactor:broken>
  (make-mactor:broken problem)
  mactor:broken?
  (problem mactor:broken-problem))

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
  (procedure redefinable-object-constructor set-redefinable-object-constructor!)
  (rehydrator redefinable-object-rehydrator set-redefinable-object-rehydrator!))

(define* (make-redefinable-object constructor #:optional rehydrator)
  "Construct a redefinable object for CONSTRUCTOR

Optionally, REHYDRATOR may be provided, which is a procedure for restoring
a persisted version of an object spawned via CONSTRUCTOR."
  (_make-redefinable-object constructor rehydrator))

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
