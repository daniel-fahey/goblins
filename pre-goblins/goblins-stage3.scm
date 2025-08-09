;;; Copyright 2019-2021 Christine Lemmer-Webber
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


;; STAGE 3: Add:
;;  - <-np
;;  - most of the mactors
;;
;; Breaking things into stages is starting to get a bit messy as I run
;; out of time...

(define-module (pre-goblins stage3)
  #:export (make-whactormap
            make-actormap

            spawn $

            actormap-spawn
            actormap-spawn!
            ;; actormap-spawn-mactor!

            actormap-turn*
            actormap-turn

            actormap-peek
            actormap-poke!
            actormap-reckless-poke!

            actormap-run
            actormap-run!
            actormap-run*

            whactormap?
            transactormap?
            transactormap-merge!

            <-np
            ;;;; yet to come:
            ;; <- on
            )
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match))


;;;                  .============================.
;;;                  | High level view of Goblins |
;;;                  '============================'
;;;
;;; There's a lot of architecture here.
;;; It's a lot to take in, so let's start out with a "high level view".
;;; Here's an image to get started:
;;;
;;;   .----------------------------------.         .-------------------.
;;;   |              Peer 1              |         |       Peer 2      |
;;;   |             =======              |         |       =======     |
;;;   |                                  |         |                   |
;;;   | .--------------.  .---------.   .-.       .-.                  |
;;;   | |    Vat A     |  |  Vat B  |   |  \______|  \_   .----------. |
;;;   | |  .---.       |  |   .-.   | .-|  /      |  / |  |   Vat C  | |
;;;   | | (Alice)----------->(Bob)----' '-'       '-'  |  |  .---.   | |
;;;   | |  '---'       |  |   '-'   |    |         |   '--->(Carol)  | |
;;;   | |      \       |  '----^----'    |         |      |  '---'   | |
;;;   | |       V      |       |         |         |      |          | |
;;;   | |      .----.  |       |        .-.       .-.     |  .----.  | |
;;;   | |     (Alfred) |       '-------/  |______/  |____---(Carlos) | |
;;;   | |      '----'  |               \  |      \  |     |  '----'  | |
;;;   | |              |                '-'       '-'     '----------' |
;;;   | '--------------'                 |         |                   |
;;;   |                                  |         |                   |
;;;   '----------------------------------'         '-------------------'
;;;
;;; Here we see the following:
;;;
;;;  - Zooming in the farthest, we are looking at the "object layer"...
;;;    Alice has a reference to Alfred and Bob, Bob has a reference to Carol,
;;;    Carlos has a reference to Bob.  Reference possession is directional;
;;;    even though Alice has a reference to Bob, Bob does not have a
;;;    reference to Alice.
;;;
;;;  - One layer up is the "vat layer"... here we can see that Alice and
;;;    Alfred are both objects in Vat A, Bob is an object in Vat B, and
;;;    Carol and Carlos are objects in Vat C.
;;;
;;;  - Zooming out the farthest is the "peer/network level".
;;;    There are two peers (Peer 1 and Peer 2) connected over a
;;;    Goblins CapTP network.  The stubby shapes on the borders between the
;;;    peers represent the directions of references Peer 1 has to
;;;    objects in Peer 2 (at the top) and references Peer 2 has to
;;;    Peer 1.  Both peers in this diagram are cooperating to preserve
;;;    that Bob has access to Carol but that Carol does not have access to
;;;    Bob, and that Carlos has access to Bob but Bob does not have access
;;;    to Carlos.  (However there is no strict guarantee from either
;;;    peer's perspective that this is the case... generally it's in
;;;    everyone's best interests to take a "principle of least authority"
;;;    approach though so usually it is.)
;;;
;;; This illustration is what's sometimes called a "grannovetter diagram"
;;; in the ocap community, inspired by the kinds of diagrams in Mark
;;; S. Grannovetter's "The Strength of Weak Ties" paper.  The connection is
;;; that while the "Weak Ties" paper was describing the kinds of social
;;; connections between people (Alice knows Bob, Bob knows Carol), similar
;;; patterns arise in ocap systems (the object Alice has a refernce to Bob,
;;; and Bob has a reference to Carol).
;;;
;;; With that in mind, we're now ready to look at things more structurally.
;
;
;                  .============================.
;                  | Goblins abstraction layers |
;                  '============================'
;
; Generally, things look like so:
;
;;;   (peer (vat (actormap {refr: (mactor object-handler)})))
;;;
;;; However, we could really benefit from looking at those in more detail,
;;; so from the outermost layer in...
;;;
;;;    .--- A peer in Goblins is basically an OS process.
;;;    |    However, the broader Goblins CapTP/MachineTP network is
;;;    |    made up of many peers.  A connection to another peer
;;;    |    is the closest amount of "assurance" a Goblins peer has
;;;    |    that it is delivering to a specific destination.
;;;    |    Nonetheless, Goblins users generally operate at the object
;;;    |    reference level of abstraction, even across peers.
;;;    |
;;;    |    An object reference on the same peer is considered
;;;    |    "local" and an object reference on another peer is
;;;    |    considered "remote".
;;;    |
;;;    |    .--- Christine: "How about I call this 'hive'?"
;;;    |    |    Ocap community: "We hate that, use 'vat'"
;;;    |    |    Everyone else: "What's a 'vat' what a weird name"
;;;    |    |
;;;    |    |    A vat is a traditional ocap term, both a container for
;;;    |    |    objects but most importantly an event loop that
;;;    |    |    communicates with other event loops.  Vats operate
;;;    |    |    "one turn at a time"... a toplevel message is handled
;;;    |    |    for some object which is transactional; either it happens
;;;    |    |    or, if something bad happens in-between, no effects occur
;;;    |    |    at all (except that a promise waiting for the result of
;;;    |    |    this turn is broken).
;;;    |    |
;;;    |    |    Objects in the same vat are "near", whereas objects in
;;;    |    |    remote vats are "far".  (As you may notice, "near" objects
;;;    |    |    can be "near" or "far", but "remote" objects are always
;;;    |    |    "far".)
;;;    |    |
;;;    |    |    This distinction is important, because Goblins supports
;;;    |    |    both asynchronous messages + promises via `<-` and
;;;    |    |    classic synchronous call-and-return invocations via `$`.
;;;    |    |    However, while any actor can call any other actor via
;;;    |    |    <-, only near actors may use $ for synchronous call-retun
;;;    |    |    invocations.  In the general case, a turn starts by
;;;    |    |    delivering to an actor in some vat a message passed with <-,
;;;    |    |    but during that turn many other near actors may be called
;;;    |    |    with $.  For example, this allows for implementing transactional
;;;    |    |    actions as transferring money from one account/purse to another
;;;    |    |    with $ in the same vat very easily, while knowing that if
;;;    |    |    something bad happens in this transaction, no actor state
;;;    |    |    changes will be committed (though listeners waiting for
;;;    |    |    the result of its transaction will be informed of its failure);
;;;    |    |    ie, the financial system will not end up in a corrupt state.
;;;    |    |    In this example, it is possible for users all over the network
;;;    |    |    to hold and use purses in this vat, even though this vat is
;;;    |    |    responsible for money transfer between those purses.
;;;    |    |    For an example of such a financial system in E, see
;;;    |    |    "An Ode to the Grannovetter Diagram":
;;;    |    |      http://erights.org/elib/capability/ode/index.html
;;;    |    |
;;;    |    |    .--- Earlier we said that vats are both an event loop and
;;;    |    |    |    a container for storing actor state.  Surprise!  The
;;;    |    |    |    vat is actually wrapping the container, which is called
;;;    |    |    |    an "actormap".  While vats do not expose their actormaps,
;;;    |    |    |    Goblins has made a novel change by allowing actormaps to
;;;    |    |    |    be used as independent first-class objects.  Most users
;;;    |    |    |    will rarely do this, but first-class usage of actormaps
;;;    |    |    |    is still useful if integrating Goblins with an existing
;;;    |    |    |    event loop (such as one for a video game or a GUI) or for
;;;    |    |    |    writing unit tests.
;;;    |    |    |
;;;    |    |    |    The keys to actormaps are references (called "refrs")
;;;    |    |    |    and the values are current behavior.  This is described
;;;    |    |    |    below.
;;;    |    |    |
;;;    |    |    |    Actormaps also technically operate on "turns", which are
;;;    |    |    |    a transactional operation.  Once a turn begins, a dynamic
;;;    |    |    |    "syscaller" (or "actor context") is initialized so that
;;;    |    |    |    actors can make changes within this transaction.  At the
;;;    |    |    |    end of the turn, the user of actormap-turn is presented
;;;    |    |    |    with the transactional actormap (called "transactormap")
;;;    |    |    |    which can either be committed or not to the current mutable
;;;    |    |    |    actormap state ("whactormap", which stands for
;;;    |    |    |    "weak hash actormap"), alongside a queue of messages that
;;;    |    |    |    were scheduled to be run from actors in this turn using <-,
;;;    |    |    |    and the result of the computation run.
;;;    |    |    |
;;;    |    |    |    However, few users will operate using actormap-turn directly,
;;;    |    |    |    and will instead either use actormap-poke! (which automatically
;;;    |    |    |    commits the transaction if it succeeds or propagates the error)
;;;    |    |    |    or actormap-peek (which returns the result but throws away the
;;;    |    |    |    transaction; useful for getting a sense of what's going on
;;;    |    |    |    without committing any changes to actor state).
;;;    |    |    |    Or, even more commonly, they'll just use a vat and never think
;;;    |    |    |    about actormaps at all.
;;;    |    |    |
;;;    |    |    |         .--- A reference to an object or actor.
;;;    |    |    |         |    Traditionally called a "ref" by the ocap community, but
;;;    |    |    |         |    scheme already uses "-ref" everywhere so we call it
;;;    |    |    |         |    "refr" instead.  Whatever.
;;;    |    |    |         |
;;;    |    |    |         |    Anyway, these are the real "capabilities" of Goblins'
;;;    |    |    |         |    "object capability system".  Holding onto one gives you
;;;    |    |    |         |    authority to make invocations with <- or $, and can be
;;;    |    |    |         |    passed around to procedure or actor invocations.
;;;    |    |    |         |    Effectively the "moral equivalent" of a procedure
;;;    |    |    |         |    reference.  If you have it, you can use (and share) it;
;;;    |    |    |         |    if not, you can't.
;;;    |    |    |         |
;;;    |    |    |         |    Actually, technically these are local-live-refrs...
;;;    |    |    |         |    see "The World of Refrs" below for the rest of them.
;;;    |    |    |         |
;;;    |    |    |         |      .--- We're now at the "object behavior" side of
;;;    |    |    |         |      |    things.  I wish I could avoid talking about
;;;    |    |    |         |      |    "mactors" but we're talking about the actual
;;;    |    |    |         |      |    implementation here so... "mactor" stands for
;;;    |    |    |         |      |    "meta-actor", and really there are a few
;;;    |    |    |         |      |    "core kinds of behavior" (mainly for promises
;;;    |    |    |         |      |    vs object behavior).  But in the general case,
;;;    |    |    |         |      |    most objects from a user's perspective are the
;;;    |    |    |         |      |    mactor:object kind, which is just a wrapper
;;;    |    |    |         |      |    around the current object handler (as well as
;;;    |    |    |         |      |    some information to track when this object is
;;;    |    |    |         |      |    "becoming" another kind of object.
;;;    |    |    |         |      |
;;;    |    |    |         |      |      .--- Finally, "object"... a term that is
;;;    |    |    |         |      |      |    unambiguous and well-understood!  Well,
;;;    |    |    |         |      |      |    "object" in our system means "references
;;;    |    |    |         |      |      |    mapping to an encapsulation of state".
;;;    |    |    |         |      |      |    Refrs are the reference part, so
;;;    |    |    |         |      |      |    object-handlers are the "current state"
;;;    |    |    |         |      |      |    part.  The time when an object transitions
;;;    |    |    |         |      |      |    from "one" behavior to another is when it
;;;    |    |    |         |      |      |    returns a new handler wrapped in a "become"
;;;    |    |    |         |      |      |    wrapper specific to this object (and
;;;    |    |    |         |      |      |    provided to the object at construction
;;;    |    |    |         |      |      |    time)
;;;    |    |    |         |      |      |
;;;    V    V    V         V      V      V
;;; (peer (vat (actormap {refr: (mactor object-handler)})))
;;;
;;;
;;; Whew!  That's a lot of info, so go take a break and then we'll go onto
;;; the next section.
;;;
;;;
;;;                     .====================.
;;;                     | The World of Refrs |
;;;                     '===================='
;;;
;;; There are a few kinds of references, explained below:
;;;
;;;                                     live refrs :
;;;                     (runtime or captp session) : offline-storeable
;;;                     ========================== : =================
;;;                                                :
;;;                local?           remote?        :
;;;           .----------------.----------------.  :
;;;   object? | local-object   | remote-object  |  :    [sturdy refrs]
;;;           |----------------+----------------|  :
;;;  promise? | local-promise  | remote-promise |  :     [cert chains]
;;;           '----------------'----------------'  :
;;;
;;; On the left hand side we see live references (only valid within this
;;; process runtime or between peers across captp sessions) and
;;; offline-storeable references (sturdy refrs, a kind of bearer URI,
;;; and certificate chains, which are like "deeds" indicating that the
;;; possessor of some cryptographic material is permitted access).
;;;
;;; All offline-storeable references must first be converted to live
;;; references before they can be used (authority to do this itself a
;;; capability, as well as authority to produce these offline-storeable
;;; objects).
;;;
;;; Live references subdivide into local (on the same peer) and
;;; remote (on a foreign peer).  These are typed as either
;;; representing an object or a promise.
;;;
;;; (Local references also further subdivide into "near" and "far",
;;; but rather than being encoded in the reference type this is
;;; determined relative to another local-refr or the current actor
;;; context.)


;; Actormaps, etc
;; ==============

(define-record-type <actormap>
  ;; TODO: This is confusing, naming-wise? (see make-actormap alias)
  (_make-actormap metatype data vat-connector)
  actormap?
  (metatype actormap-metatype)
  (data actormap-data)
  (vat-connector actormap-vat-connector))

(set-record-type-printer!
 <actormap>
 (lambda (am port)
   (format port "#<actormap ~a>" (actormap-metatype-name (actormap-metatype am)))))

(define-record-type <actormap-metatype>
  (make-actormap-metatype name ref-proc set!-proc)
  actormap-metatype?
  (name actormap-metatype-name)
  (ref-proc actormap-metatype-ref-proc)
  (set!-proc actormap-metatype-set!-proc))

(define (actormap-set! am key val)
  ((actormap-metatype-set!-proc (actormap-metatype am))
   am key val)
  *unspecified*)

;; (-> actormap? local-refr? (or/c mactor? #f))
(define (actormap-ref am key)
  ((actormap-metatype-ref-proc (actormap-metatype am)) am key))

;; Weak-hash actormaps
;; ===================

(define-record-type <whactormap-data>
  (make-whactormap-data wht)
  whactormap-data?
  (wht whactormap-data-wht))

(define (whactormap-ref am key)
  (define wht (whactormap-data-wht (actormap-data am)))
  (hashq-ref wht key #f))

(define (whactormap-set! am key val)
  (define wht (whactormap-data-wht (actormap-data am)))
  (hashq-set! wht key val))

(define whactormap-metatype
  (make-actormap-metatype 'whactormap whactormap-ref whactormap-set!))

(define* (make-whactormap #:key [vat-connector #f])
  (_make-actormap whactormap-metatype
                  (make-whactormap-data (make-weak-key-hash-table))
                  vat-connector))

(define (whactormap? obj)
  (and (actormap? obj)
       (eq? (actormap-metatype obj) whactormap-metatype)))

;; TODO: again, confusing (see <actormap>)
(define make-actormap make-whactormap)



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

(define (transactormap-ref transactormap key)
  (define tm-data (actormap-data transactormap))
  (when (transactormap-data-merged? tm-data)
    (error "Can't use transactormap-ref on merged transactormap"))
  (define tm-delta
    (transactormap-data-delta tm-data))
  (define tm-val (hashq-ref tm-delta key #f))
  (if tm-val
      ;; we got it, it's in our delta
      tm-val
      ;; search parents for key
      (let ([parent (transactormap-data-parent tm-data)])
        (actormap-ref parent key))))

(define (transactormap-set! transactormap key val)
  (when (transactormap-merged? transactormap)
    (error "Can't use transactormap-set! on merged transactormap"))
  (define tm-delta (transactormap-data-delta (actormap-data transactormap)))
  (hashq-set! tm-delta key val)
  *unspecified*)

;; Not threadsafe, but probably doesn't matter
(define (transactormap-merge! transactormap)
  ;; Serves two functions:
  ;;  - to extract the root weak-hasheq
  ;;  - to merge this transaction on top of the weak-hasheq
  (define (do-merge! transactormap)
    (define tm-data (actormap-data transactormap))
    (define parent (transactormap-data-parent tm-data))
    (define parent-mtype (actormap-metatype parent))
    ;; TODO: Should we actually return the root-wht instead,
    ;;   since that's what we're comitting to?
    (define root-actormap
      (cond
       [(eq? parent-mtype whactormap-metatype)
        parent]
       [(eq? parent-mtype transactormap)
        (do-merge! parent)]
       [else
        (error (format #f "Actormap metatype not supported for merging: ~a"
                       parent-mtype))]))
    ;; Optimization: we pull out the root weak hash table here and
    ;; merge it
    (define root-wht (whactormap-data-wht (actormap-data root-actormap)))
    (unless (transactormap-data-merged? tm-data)
      (hash-for-each
       (lambda (key val)
         (hashq-set! root-wht key val))
       (transactormap-data-delta tm-data))
      (set-transactormap-data-merged?! tm-data #t))
    root-actormap)
  (do-merge! transactormap)
  *unspecified*)

(define transactormap-metatype
  (make-actormap-metatype 'transactormap transactormap-ref transactormap-set!))

(define (make-transactormap parent)
  (define vat-connector (actormap-vat-connector parent))
  (_make-actormap transactormap-metatype
                  (make-transactormap-data parent (make-hash-table) #f)
                  vat-connector))



;; Ref(r)s
;; =======

(define-record-type <local-object-refr>
  (make-local-object-refr debug-name vat-connector)
  local-object-refr?
  (debug-name local-object-refr-debug-name)
  (vat-connector local-object-refr-vat-connector))

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
  (or (local-object-refr? obj) (local-promise-refr? obj)))

(define (local-refr-vat-connector local-refr)
  (match local-refr
    [(? local-object-refr?)
     (local-object-refr-vat-connector local-refr)]
    [(? local-promise-refr?)
     (local-promise-refr-vat-connector local-refr)]))


(define (live-refr? obj)
  (or (local-refr? obj)
      ;; TODO: Finish as we fill in the other refr types
      ))

#;(define (actormap-poke! am refr . args)
  'TODO)



;; "Become" sealer/unsealers
;; =========================

(define (make-become-sealer-triplet)
  (define-record-type <become-seal>
    (make-become-seal new-behavior return-val)
    become-sealed?
    (new-behavior unseal-behavior)
    (return-val unseal-return-val))
  (define* (become new-behavior #:optional [return-val *unspecified*])
    (make-become-seal new-behavior return-val))
  (define (unseal sealed)
    (values (unseal-behavior sealed)
            (unseal-return-val sealed)))
  (values become unseal become-sealed?))



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
;;;        Unresolved                     Resolved
;;;  __________________________  ___________________________
;;; |                          ||                           |
;;;
;;;                 .----------------->.    .-->[object]
;;;                 |                  |    |
;;;                 |    .--.          |    +-->[local-link]
;;;     [naive]-->. |    v  |          |    |            
;;;               +>+->[closer]------->'--->+-->[encased]
;;;  [question]-->' |       |               |            
;;;                 |       |               '-->[broken]
;;;                 '------>'--->[remote-link]    ^
;;;                                  |            |
;;;                                  '----------->'
;;;
;;; |________________________________________||_____________|
;;;                  Eventual                     Settled
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
;;; (remote-refrs of course never correspond to a mactor on this machine;
;;; those are managed by captp.)
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
  (mactor:object behavior become-unsealer become?)
  mactor:object?
  (behavior mactor:object-behavior)
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
;; It knows no interesting information about how what it will eventually
;; become.
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

;; Link to an object on the same machine.
(define-record-type <mactor:local-link>
  (make-mactor:local-link point-to)
  mactor:local-link?
  (point-to mactor:local-link-point-to))

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

;; Rather than directly storing references to listeners, we use these
;; <listener-info> structs because, at least at the time, we have this
;; notion of being interested in "partial" updates (rather than waiting
;; until full promise resolution)
;;
;; While this is a curious feature, we never fully documented why we
;; made the decision to enable this.  It would be interesting to document
;; it, and we probably will indeed need to for ocapn interoperability.
(define-record-type <listener-info>
  (make-listener-info resolve-me wants-partial?)
  listener-info?
  (resolve-me listener-info-resolve-me)
  (wants-partial? listener-info-wants-partial?))

(define (mactor:eventual? obj)
  (or (mactor:remote-link? obj)
      (mactor:unresolved? obj)))
(define (mactor:unresolved? obj)
  (or (mactor:naive? obj)
      (mactor:question? obj)
      (mactor:closer? obj)))

(define (mactor-get-m~unresolved obj)
  (match obj
    [(? mactor:naive?) (mactor:naive-unresolved obj)]
    [(? mactor:question?) (mactor:question-unresolved obj)]
    [(? mactor:closer?) (mactor:closer-unresolved obj)]))

(define (mactor-get-m~eventual obj)
  (match obj
    [(? mactor:unresolved? obj)
     (m~unresolved-eventual (mactor-get-m~unresolved obj))]
    [(? mactor:remote-link? obj)
     (mactor:remote-link-eventual obj)]))

(define (mactor:unresolved-add-listener mactor new-listener wants-partial?)
  (define new-listener-info
    (make-listener-info new-listener wants-partial?))
  (define new-unresolved
    (match (mactor-get-m~unresolved mactor)
      [($ <m~unresolved> eventual listeners)
       (make-m~unresolved eventual (cons new-listener-info listeners))]))
  (match mactor
    [($ <mactor:naive> unresolved waiting-messages)
     (make-mactor:naive new-unresolved waiting-messages)]
    [($ <mactor:question> unresolved captp-connector question-finder)
     (make-mactor:question new-unresolved captp-connector question-finder)]
    [($ <mactor:closer> unresolved point-to history waiting-messages)
     (make-mactor:closer new-unresolved point-to history waiting-messages)]))

;; Helper for syscaller's fulfill-promise and break-promise methods
(define (unseal-mactor-resolution mactor sealed-resolution)
  (define eventual (mactor-get-m~eventual mactor))
  (define resolver-tm?
    (m~eventual-resolver-tm? eventual))
  (define resolver-unsealer
    (m~eventual-resolver-unsealer eventual))
  ;; Is this a valid resolution?
  (unless (resolver-tm? sealed-resolution)
    (error "Resolution sealed with wrong trademark!"))
  (resolver-unsealer sealed-resolution))

;; (define (near-refr? refr)
;;   (define sys (get-syscaller-or-die))
;;   (sys 'near-refr? refr))
;; (define (far-refr? refr)
;;   (not (near-refr? refr)))

;; ;; Dangerous and dynamic... not intended to be exposed outside of here
;; ;; at this time, anyway.
;; ;; Used to implement some promise-introspection methods...
;; (define (near-mactor refr)
;;   (-> near-refr? any/c)
;;   ((current-syscaller) 'near-mactor refr))



;; Messages
;; --------

;; These are the main things that get sent as the toplevel of a turn in a vat!
(define-record-type <message>
  (make-message to resolve-me args answer-this-question)
  message?
  ;; who's receiving the message (the invoked actor)
  (to message-to)
  ;; who's interested in the result (a resolver)
  (resolve-me message-resolve-me)
  ;; arguments to the invoked actor
  (args message-args)
  ;; Either a question-finder or #f
  (answer-this-question message-answer-this-question))

(define (question-message? msg)
  (if (message-answer-this-question msg) #t #f))

;; ;; Sent in the same way as <message>, but does listen requests specifically
;; (define-record-type <listen-request>
;;   (make-listen-request to listener wants-partial?)
;;   listen-request?
;;   (to listen-request-to)
;;   (listener listen-request-listener)
;;   (wants-partial? listen-request-wants-partial?))



;; Re-entry Protection
;; ===================
;;
;; Or rather, re-entry protection goes here.
;; This works fairly ideally in Racket; in Guile the
;; with-continuation-barrier version below... well it's complicated because
;; unlike in Racket, it's not "marking the stack" to prevent re-entry but
;; permitting exceptions to "move upward" as it were... so we have to do
;; that ourselves, and it's kind of a mess.  No idea what the performance
;; implications are.

(define %re-entry-protect (make-parameter #f))

(define (_re-protec proc)
  (define result
    ;; protect against re-entrancy attacks
    ;; (... but also "protects" against live debugging, unfortunately)
    (with-continuation-barrier
     (lambda ()
       (with-exception-handler
           (lambda (exn)
             (list 'error exn))
         (lambda ()
           ;; actors are only permitted one value from their
           ;; continuation
           (define result
             (proc))
           (list 'success result))
         #:unwind? #t
         #:unwind-for-type #t))))
  (match result
    [('success result)
     result]
    [('error err)
     (raise-exception err)]))

(define (with-re-entry-protection proc)
  (if (%re-entry-protect)
      (_re-protec proc)
      (proc)))



;; Syscaller
;; =========

;; Do NOT export this esp under serious ocap confinement
(define current-syscaller (make-parameter #f))

(define (fresh-syscaller actormap)
  (define vat-connector
    (actormap-vat-connector actormap))
  (define new-msgs '())

  (define closed? #f)

  (define (this-syscaller method-id . args)
    (when closed?
      (error "Sorry, this syscaller is closed for business!"))
    (define method
      (case method-id
        [($) _$]
        [(spawn) _spawn]
        ;; [(<-) _<-]
        [(<-np) _<-np]
        [(spawn-mactor) spawn-mactor]
        [(send-message) _send-message]
        ;; TODO:
        ;; [(fulfill-promise) fulfill-promise]
        ;; [(break-promise) break-promise]
        ;; [(handle-message) _handle-message]
        ;; [(handle-listen) _handle-listen]
        ;; [(send-listen) _send-listen]
        ;; [(on) _on]
        [(vat-connector) get-vat-connector]
        [(near-refr?) near-refr?]
        [(near-mactor) near-mactor]
        [else (error 'invalid-syscaller-method
                     "~a" method-id)]))
    (apply method args))

  (define (near-refr? obj)
    (and (local-refr? obj)
         (eq? (local-refr-vat-connector obj)
              vat-connector)))

  (define (near-mactor refr)
    (actormap-ref actormap refr))

  (define (get-vat-connector)
    vat-connector)

  (define (actormap-ref-or-die to-refr)
    (define mactor
      (actormap-ref actormap to-refr))
    (unless mactor
      (error 'no-such-actor "no actor with this id in this vat: ~a" to-refr))
    mactor)

  ;; call actor's behavior
  (define (_$ to-refr args)
    ;; Restrict to live-refrs which appear to have the same
    ;; vat-connector as us
    (unless (local-refr? to-refr)
      (error 'not-callable
             "Not a live reference: ~a" to-refr))

    (unless (eq? (local-refr-vat-connector to-refr)
                 vat-connector)
      (error 'not-callable
             "Not in the same vat: ~a" to-refr))

    (define mactor
      (actormap-ref-or-die to-refr))

    (match mactor
      [(? mactor:object?)
       (let ((actor-behavior
              (mactor:object-behavior mactor))
             (become?
              (mactor:object-become? mactor))
             (become-unsealer
              (mactor:object-become-unsealer mactor)))
         ;; I guess watching for this guarantees that an immediate call
         ;; against a local actor will not be tail recursive.
         ;; TODO: We need to document that.
         (define-values (new-behavior return-val)
           (let ([returned
                  (with-re-entry-protection
                   (lambda ()
                     (apply actor-behavior args)))])
             (if (become? returned)
                 ;; The unsealer unseals both the behavior and return-value anyway
                 (become-unsealer returned)
                 ;; In this case, we're not becoming anything, so just give us
                 ;; the return-val
                 (values #f returned))))

         ;; if a new behavior for this actor was specified,
         ;; let's replace it
         (when new-behavior
           (unless (procedure? new-behavior)
             (error 'become-failure "Tried to become a non-procedure behavior:"
                    new-behavior))
           (actormap-set! actormap to-refr
                          (mactor:object
                           new-behavior
                           (mactor:object-become-unsealer mactor)
                           (mactor:object-become? mactor))))

         return-val)]
      ;; If it's an encased value, "calling" it just returns the
      ;; internal value.
      [(? mactor:encased?)
       (mactor:encased-val mactor)]
      ;; Ah... we're linking to another actor locally, so let's
      ;; just de-symlink and call that instead.
      [(? mactor:local-link?)
       (apply _$ (mactor:local-link-point-to mactor)
              args)]
      ;; Not a callable mactor!
      [_other
       (error 'not-callable
              "Not an encased or object mactor: ~a" mactor)]))

  ;; spawn a new actor
  (define (_spawn constructor args debug-name)
    (define-values (become become-unsealer become-sealed?)
      (make-become-sealer-triplet))
    (define initial-behavior
      (apply constructor become args))
    (match initial-behavior
      ;; New procedure, so let's set it
      [(? procedure?)
       (let ((actor-refr
              (make-local-object-refr debug-name vat-connector)))
         (actormap-set! actormap actor-refr
                        (mactor:object initial-behavior
                                       become-unsealer become-sealed?))
         actor-refr)]
      ;; If someone returns another actor, just let that be the actor
      [(? live-refr? pre-existing-refr)
       pre-existing-refr]
      [_
       (error 'invalid-actor-handler "Not a procedure or live refr: ~a" initial-behavior)]))

  (define (spawn-mactor mactor debug-name)
    (actormap-spawn-mactor! actormap mactor debug-name))

  #;(define (fulfill-promise promise-id sealed-val)
    (call/ec
     (lambda (return-early)
       (define orig-mactor
         (actormap-ref-or-die promise-id))
       (unless (mactor:unresolved? orig-mactor)
         (error 'resolving-resolved
                "Attempt to resolve resolved actor: ~a" promise-id))
       (define resolve-to-val
         (unseal-mactor-resolution orig-mactor sealed-val))

       (define orig-waiting-messages
         (match orig-mactor
           [(? mactor:naive?)
            (mactor:naive-waiting-messages orig-mactor)]
           [(? mactor:closer?)
            (mactor:closer-waiting-messages orig-mactor)]
           [_ '()]))

       (define (forward-messages [waiting-messages orig-waiting-messages])
         (match waiting-messages
           ['() (void)]
           [(list (message _old-to resolve-me kws kw-vals args)
                  rest-waiting ...)
            ;; preserve FIFO by recursing first
            (forward-messages rest-waiting)
            ;; shouldn't be a question message so we don't need to
            ;; #:answer-this-question, I think?
            (_send-message kws kw-vals resolve-to-val resolve-me args)]))

       (define new-waiting-messages
         (if (remote-promise-refr? resolve-to-val)
             ;; don't forward waiting messages to remote promises
             orig-waiting-messages
             ;; but do forward to literally anything else... empty
             ;; the queue!
             (begin (forward-messages)
                    '())))

       (define orig-listeners
         (mactor:unresolved-listeners orig-mactor))

       (define next-mactor-state
         (match resolve-to-val
           [(? local-object-refr?)
            (when (eq? resolve-to-val promise-id)
              (return-early
               ;; We want to break this because it should be explicitly clear
               ;; to everyone that the promise was broken.
               (break-promise promise-id
                              ;; TODO: we need some sort of error type we do
                              ;;   allow to explicitly be shared, this one is a
                              ;;   reasonable candidate
                              'cycle-in-promise-resolution)))
            (mactor:local-link resolve-to-val)]
           [(? remote-object-refr?)
            ;; Since the captp connection is the one that might break this,
            ;; we need to ask it what it uses as its resolver unsealer/tm
            ;; @@: ... This doesn't seem like a good solution.
            ;;   Whatever, we need to add when-broken or something.
            (match-define (cons new-resolver-unsealer new-resolver-tm?)
              (let ([connector (remote-refr-captp-connector resolve-to-val)])
                ;; TODO: Do we need to notify it that we want to know about
                ;;   breakage?  Presumably... so do it here instead...?
                (connector 'partition-unsealer-tm-cons)))

            (mactor:remote-link new-resolver-unsealer new-resolver-tm?
                                resolve-to-val)]
           [(or (? local-promise-refr?)
                (? remote-promise-refr?))
            (define new-history
              (if (mactor:closer? orig-mactor)
                  (set-add (mactor:closer-history orig-mactor)
                           (mactor:closer-point-to orig-mactor))
                  (seteq promise-id)))
            ;; Detect cycles!
            (when (set-member? new-history resolve-to-val)
              ;; not sure we actually need to return anything, but I guess
              ;; this is mildly future-proof.
              (return-early
               ;; We want to break this because it should be explicitly clear
               ;; to everyone that the promise was broken.
               (break-promise promise-id
                              ;; TODO: we need some sort of error type we do
                              ;;   allow to explicitly be shared, this one is a
                              ;;   reasonable candidate
                              'cycle-in-promise-resolution)))

            ;; Make a new set of resolver sealers for this.
            ;; However, we don't use the general ^resolver because we're
            ;; explicitly using the fulfilled-handler/broken-handler things
            (define-values (new-resolver-sealer new-resolver-unsealer new-resolver-tm?)
              (make-sealer-triplet 'fulfill-promise))
            (define new-resolver
              (_spawn ^resolver '() '() (list promise-id new-resolver-sealer)))
            ;; Now subscribe to the promise...
            (_send-listen resolve-to-val new-resolver #t)
            ;; Now we want to both inform any listeners that are interested
            ;; in partial information and scrub them out of the current
            ;; listeners list.
            (define new-listeners
              (for/fold ([new-listeners '()]
                         #:result (reverse new-listeners))
                        ([listener-info orig-listeners])
                (if (listener-info-wants-partial? listener-info)
                    ;; resolve and drop out of listeners
                    (begin (_<-np (listener-info-resolve-me listener-info)
                                  'fulfill resolve-to-val)
                           new-listeners)
                    (cons listener-info new-listeners))))
            ;; Now we become "closer" to this promise
            (mactor:closer new-resolver-unsealer new-resolver-tm?
                           new-listeners
                           resolve-to-val new-history
                           new-waiting-messages)]
           ;; anything else is an encased value
           [_ (mactor:encased resolve-to-val)]))

       ;;  - Now actually switch to the new mactor state
       (actormap-set! actormap promise-id
                      next-mactor-state)

       ;; Resolve listeners, if appropriate (ie, if not mactor:closer)
       (unless (mactor:unresolved? next-mactor-state)
         (for ([listener-info orig-listeners])
           (<-np (listener-info-resolve-me listener-info)
                 'fulfill resolve-to-val))))))

  ;; TODO: Add support for broken-because-of-network-partition support
  ;;   even for mactor:remote-link
  #;(define (break-promise promise-id sealed-problem)
    (match (actormap-ref actormap promise-id #f)
      ;; TODO: Not just local-promise, anything that can
      ;;   break
      [(? mactor:unresolved? unresolved-mactor)
       (define problem
         (unseal-mactor-resolution unresolved-mactor sealed-problem))
       ;; Now we "become" broken with that problem
       (actormap-set! actormap promise-id
                      (mactor:broken problem))
       ;; Inform all listeners of the resolution
       (for ([listener-info (in-list (mactor:unresolved-listeners unresolved-mactor))])
         (<-np (listener-info-resolve-me listener-info)
               'break problem))]
      [(? mactor:remote-link?)
       (error "TODO: Implement breaking on captp disconnect!")]
      [#f (error "no actor with this id")]
      [_ (error "can only resolve eventual references")]))


  ;; Note that _handle-message is really, seriously for handling *toplevel*
  ;; messages... ie, turns.
  ;; This is the bulk of what's called and handled by actormap-turn-message.
  ;; (As opposed to actormap-turn*, which only supports calling, this also
  ;; handles any toplevel invocation of an actor, probably via message send.)
  #;(define (_handle-message msg display-or-log-error)
    (match-define (message to-refr resolve-me kws kw-vals args)
      msg)
    (unless (near-refr? to-refr)
      (error 'not-a-near-refr "Not a near refr: ~a" to-refr))

    (define orig-mactor
      (actormap-ref-or-die to-refr))

    ;; Prevent someone trying to throw this vat into an infinite loop
    (when (eq? to-refr resolve-me)
      (error 'same-recipient-and-resolver
             "Recipient and resolver are the same: ~a" to-refr))

    (define (call-with-resolution proc)
      (with-handlers ([exn:fail?
                       (lambda (err)
                         (when display-or-log-error
                           (display-or-log-error err))
                         ;; We need to revert any messages that were going
                         ;; to send to preserve transactionality
                         (set! new-msgs '())
                         ;; ... but we're still going to send this one
                         (when resolve-me
                           (_<-np resolve-me 'break err))
                         `#(fail ,err))])
        (define call-result
          (proc))
        (when resolve-me
          (_<-np resolve-me 'fulfill call-result))
        `#(success ,call-result)))

    (match orig-mactor
      ;; If it's callable, we just use the call behavior, because
      ;; that's effectively the same code we'd be running anyway.
      ;; However, we do want to handle the resolution.
      [(or (? mactor:object?)
           (? mactor:encased?))
       (call-with-resolution
        (lambda () (keyword-apply _$ kws kw-vals to-refr args)))]
      [(mactor:local-link point-to)
       (cond
         [(near-refr? point-to)
          (call-with-resolution
           (lambda () (keyword-apply _$ kws kw-vals point-to args)))]
         ;; it's not near so we need to pass this along
         [else
          (_send-message kws kw-vals point-to resolve-me args)
          `#(success ,(void))])]
      [(mactor:broken problem)
       (_<-np resolve-me 'break problem)
       `#(fail ,problem)]
      [(? mactor:remote-link?)
       (define point-to (mactor:remote-link-point-to orig-mactor))
       (call-with-resolution
        (lambda ()
          ;; Pass along the message
          ;; Mild optimization: only produce a promise if we have a resolver
          (keyword-apply (if resolve-me
                             _<-
                             _<-np)
                         kws kw-vals point-to args)))]
      ;; Messages sent to a promise that is "closer" are a kind of
      ;; intermediate state; we build a queue.
      [(mactor:closer resolver-unsealer resolver-tm?
                      listeners
                      point-to history
                      waiting-messages)
       (match point-to
         ;; If we're pointing at another near promise then we recurse
         ;; to _handle-messages with the next promise...
         [(? local-promise-refr?)
          ;; Now we need to see if it's in the same vat...
          (cond
            [(near-refr? point-to)
             ;; (We don't use call-with-resolution because the next one will!)
             (_handle-message (message point-to resolve-me kws kw-vals args)
                              display-or-log-error)]
            [else
             ;; Otherwise, we need to forward this message to the appropriate
             ;; vat
             (_send-message kws kw-vals point-to resolve-me args)
             `#(success ,(void))])]
         ;; But if it's a remote promise then we queue it in the waiting
         ;; messages because we prefer to have messages "swim as close
         ;; as possible to the machine barrier where possible", with
         ;; the exception of questions/answers which always cross over
         ;; (see mactor:question handling later in this procedure)
         [(? remote-promise-refr?)
          ;; Since we're queueing to send the message until it resolves
          ;; we don't resolve the problem here... hence we don't
          ;; use call-with-resolution here either.
          (actormap-set! actormap to-refr
                         (mactor:closer resolver-unsealer resolver-tm?
                                        listeners
                                        point-to history
                                        (cons msg waiting-messages)))
          ;; But we should return that this was deferred
          '#(deferred ,(void))])]
      ;; Similar to the above w/ remote promises, except that we really
      ;; just don't know where things go *at all* yet, so no swimming
      ;; occurs.
      [(mactor:naive resolver-unsealer resolver-tm?
                     listeners waiting-messages)
       (actormap-set! actormap to-refr
                      (mactor:naive resolver-unsealer resolver-tm?
                                    listeners
                                    (cons msg waiting-messages)))
       `#(deferred ,(void))]
      ;; Questions should forward their messages to the captp thread
      ;; to deal with using the relevant question-finder.
      [(? mactor:question?)
       (call-with-resolution
        (lambda ()
          (define to-question-finder
            (mactor:question-question-finder orig-mactor))
          (define captp-connector
            (mactor:question-captp-connector orig-mactor))
          (cond
            ;; If we're being asked to resolve something, this is a
            ;; "followup question"
            [resolve-me
             (define followup-question-finder
               (captp-connector 'new-question-finder))
             (define-values (followup-question-promise followup-question-resolver)
               (_spawn-promise-values #:question-finder
                                      followup-question-finder
                                      #:captp-connector
                                      captp-connector))
             (captp-connector
              'handle-message
              (question-message to-question-finder followup-question-resolver
                                kws kw-vals args
                                followup-question-finder))
             followup-question-promise]
            ;; Otherwise, we can just send it without any question and return
            ;; void
            [else
             (captp-connector
              'handle-message
              (message to-question-finder #f
                       kws kw-vals args))
             (void)])))]))

  ;; helper to the below two methods
  (define* (_send-message to-refr resolve-me args
                          #:key [answer-this-question #f])
    (unless (live-refr? to-refr)
      (error 'send-message
             "Don't know how to send a message to:" to-refr))
    (define new-message
      (if answer-this-question
          (make-message to-refr resolve-me args answer-this-question)
          (make-message to-refr resolve-me args #f)))
    (set! new-msgs (cons new-message new-msgs)))

  (define (_<-np to-refr args)
    (_send-message to-refr #f args)
    *unspecified*)

  ;; Well, this does do a bit more heavy lifting than *just* call
  ;; _send-message.
  ;;
  ;; It also constructs a promise (including, possibly, a question promise)
  #;(define _<-
    (make-keyword-procedure
     (lambda (kws kw-vals to-refr . args)
       (match to-refr
         [(? local-refr?)
          (define-values (promise resolver)
            (_spawn-promise-values))
          (_send-message kws kw-vals to-refr resolver args)
          promise]
         [(? remote-refr?)
          (define captp-connector
            (remote-refr-captp-connector to-refr))
          (define question-finder
            (captp-connector 'new-question-finder))
          (define-values (promise resolver)
            (_spawn-promise-values #:question-finder
                                   question-finder
                                   #:captp-connector
                                   captp-connector))
          (_send-message kws kw-vals to-refr resolver args
                         #:answer-this-question question-finder)
          promise]
         [to-refr
          (error 'send-message
                 "Don't know how to send a message to:" to-refr)]))))

  #;(define (_send-listen to-refr listener [wants-partial? #f])
    (match to-refr
      [(? live-refr?)
       (define listen-req
         (listen-request to-refr listener wants-partial?))
       (set! new-msgs (cons listen-req new-msgs))]
      [val (<-np listener 'fulfill val)]))

  #;(define (_handle-listen to-refr listener wants-partial? display-or-log-error)
    (with-handlers ([exn:fail?
                     (lambda (err)
                       (when display-or-log-error
                         (display-or-log-error err while-handling-listen-header))
                       `#(fail ,err))])
      (unless (near-refr? to-refr)
        (error 'not-a-near-refr "Not a near refr:" to-refr))
      (define mactor
        (actormap-ref-or-die to-refr))
      (match mactor
        [(? mactor:local-link?)
         (define point-to
           (mactor:local-link-point-to mactor))
         (if (near-refr? point-to)
             (_handle-listen (mactor:local-link-point-to mactor)
                             listener wants-partial? display-or-log-error)
             (_send-listen point-to listener wants-partial?))]
        ;; This object is a local promise, so we should handle it.
        [(? mactor:unresolved?)
         ;; Set a new version of the local-promise with this
         ;; object as a listener
         (actormap-set! actormap to-refr
                        (mactor:unresolved-add-listener mactor listener
                                                        wants-partial?))]
        ;; In the following cases we can resolve the listener immediately...
        [(? mactor:broken? mactor)
         (_<-np listener 'break (mactor:broken-problem mactor))]
        [(? mactor:encased? mactor)
         (_<-np listener 'fulfill (mactor:encased-val mactor))]
        [(? mactor:object? mactor)
         (_<-np listener 'fulfill to-refr)]
        ;; For remote links, we resolve directly to that reference
        [(? mactor:remote-link? mactor)
         (_<-np listener 'fulfill (mactor:remote-link-point-to mactor))])
      ;; return with same semantics that _handle-message does
      `#(success ,(void))))

  ;; At THIS stage, fulfilled-handler, broken-handler, finally-handler should
  ;; be actors or #f.  That's not the case in the user-facing
  ;; `on' procedure.
  #;(define (_on on-refr [fulfilled-handler #f]
               #:catch [broken-handler #f]
               #:finally [finally-handler #f]
               #:promise? [promise? #f])
    (define-values (return-promise return-p-resolver)
      (if promise?
          (spawn-promise-values)
          (values #f #f)))

    ;; These two procedures are called once the fulfillment
    ;; or break of the on-refr has actually occurred.
    (define ((handle-resolution on-resolution
                                resolve-fulfill-command) val)
      (cond [on-resolution
             ;; We can't use _send-message directly, because this may
             ;; be in a separate syscaller at the time it's resolved.
             (define syscaller (get-syscaller-or-die))
             ;; But anyway, we want to resolve the return-p-resolver with
             ;; whatever the on-resolution is, which is why we do this goofier
             ;; roundabout
             (syscaller 'send-message
                        '() '() on-resolution
                        ;; Which may be #f!
                        return-p-resolver
                        (list val))
             (when finally-handler
               (<-np finally-handler))]
            ;; There's no on-resolution, which means we can just fulfill
            ;; the promise immediately!
            [else
             (when finally-handler
               (<-np finally-handler))
             (when return-p-resolver
               (<-np return-p-resolver resolve-fulfill-command val))]))
    (define handle-fulfilled
      (handle-resolution fulfilled-handler 'fulfill))
    (define handle-broken
      (handle-resolution broken-handler 'break))

    ;; The purpose of this listener is that the promise
    ;; *hasn't resolved yet*.  Because of that we need to
    ;; queue something to happen *once* it resolves.
    (define (^on-listener bcom)
      (match-lambda*
        [(list 'fulfill val)
         (handle-fulfilled val)
         (void)]
        [(list 'break problem)
         (handle-broken problem)
         (void)]))
    (define listener
      (_spawn ^on-listener '() '() '()))
    (_send-listen on-refr listener)
    (when promise?
      return-promise))

  (define (get-internals)
    (list actormap new-msgs))

  (define (close-up!)
    (set! closed? #t))

  (values this-syscaller get-internals close-up!))

(define (call-with-fresh-syscaller am proc)
  (define-values (sys get-sys-internals close-up!)
    (fresh-syscaller am))
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (parameterize ([current-syscaller sys])
        (proc sys get-sys-internals)))
    (lambda ()
      (close-up!))))

;; In case you want to spawn PROC right off of your vat without
;; involving the syscaller at all
#;(define (syscaller-free-thread proc)
  (parameterize ((current-syscaller #f))
    (thread proc)))

(define (get-syscaller-or-die)
  (define sys (current-syscaller))
  (unless sys
    (error "No current syscaller"))
  sys)



;; Core API
;; ========

;; System calls
(define (spawn constructor . args)
  (define sys (get-syscaller-or-die))
  (sys 'spawn constructor args (procedure-name constructor)))
(define ($ refr . args)
  (define sys (get-syscaller-or-die))
  (sys '$ refr args))
(define (<- refr . args)
  (define sys (get-syscaller-or-die))
  (sys '<- refr args))
(define (<-np refr . args)
  (define sys (get-syscaller-or-die))
  (sys '<-np refr args))

(define* (on vow #:optional (fulfilled-handler #f)
             #:key
             [catch #f]
             [finally #f]
             [promise? #f])
  'TODO)

#;(define (_spawn-promise-values #:key
                               (question-finder #f)
                               (captp-connector #f))
  'TODO)

(define (spawn-promise-values)
  'TODO)
(define (spawn-promise-cons)
  'TODO)


;; ;; (define am (make-whactormap))

;; ;; (actormap-set! am 'hello 'world)



;; Spawning
;; ========

;; This is the internally used version of actormap-spawn,
;; also used by the syscaller.  It doesn't set up a syscaller
;; if there isn't currently one.
(define* (actormap-spawn!* actormap actor-constructor
                           args
                           #:optional
                           [debug-name (procedure-name actor-constructor)])
  (define vat-connector
    (actormap-vat-connector actormap))
  (define-values (become become-unseal become?)
    (make-become-sealer-triplet))
  (define actor-handler
    (apply actor-constructor become args))
  (match actor-handler
    ;; New procedure, so let's set it
    [(? procedure?)
     (let ((actor-refr
            (make-local-object-refr debug-name vat-connector)))
       (actormap-set! actormap actor-refr
                      (mactor:object actor-handler
                                     become-unseal become?))
       actor-refr)]
    [(? live-refr? pre-existing-refr)
     pre-existing-refr]
    [_
     (error 'invalid-actor-handler "Not a procedure or live refr: ~a" actor-handler)]))

;; These two are user-facing procedures.  Thus, they set up
;; their own syscaller.

;; non-committal version of actormap-spawn
(define (actormap-spawn actormap actor-constructor . args)
  (define new-actormap
    (make-transactormap actormap))
  (call-with-fresh-syscaller
   new-actormap
   (lambda (sys get-sys-internals)
     (define actor-refr
       (actormap-spawn!* new-actormap actor-constructor
                         args))
     (values actor-refr new-actormap))))

(define (actormap-spawn! actormap actor-constructor . args)
  (define new-actormap
    (make-transactormap actormap))
  (define actor-refr
    (call-with-fresh-syscaller
     new-actormap
     (lambda (sys get-sys-internals)
       (actormap-spawn!* new-actormap actor-constructor args))))
  (transactormap-merge! new-actormap)
  actor-refr)

(define* (actormap-spawn-mactor! actormap mactor
                                 #:optional
                                 [debug-name #f])
  (define vat-connector
    (actormap-vat-connector actormap))
  (define actor-refr
    (if (mactor:object? mactor)
        (make-local-object-refr debug-name vat-connector)
        (make-local-promise-refr vat-connector)))
  (actormap-set! actormap actor-refr mactor)
  actor-refr)



;;; actormap turning and utils
;;; ==========================

(define (actormap-turn* actormap to-refr args)
  (call-with-fresh-syscaller
   actormap
   (lambda (sys get-sys-internals)
     (define result-val
       (sys '$ to-refr args))
     (apply values result-val
            (get-sys-internals)))))  ; actormap new-msgs

(define (actormap-turn actormap to-refr . args)
  (define new-actormap
    (make-transactormap actormap))
  (actormap-turn* new-actormap to-refr args))

;; run a turn but only for getting the result.
;; we're not interested in committing the result
;; so we discard everything but the result.
(define (actormap-peek actormap to-refr . args)
  (define-values (returned-val _am _nm)
    (actormap-turn* (make-transactormap actormap)
                    to-refr args))
  returned-val)

;; Note that this does nothing with the messages.
(define (actormap-poke! actormap to-refr . args)
  (define-values (returned-val transactormap _nm)
    (actormap-turn* (make-transactormap actormap)
                    to-refr args))
  (transactormap-merge! transactormap)
  returned-val)

(define (actormap-reckless-poke! actormap to-refr . args)
  (define-values (returned-val _am _nm)
    (actormap-turn* actormap to-refr args))
  returned-val)

;; like actormap-run but also returns the new actormap, new-msgs
(define (actormap-run* actormap thunk)
  (define-values (actor-refr new-actormap)
    (actormap-spawn (make-transactormap actormap) (lambda (_bcom) thunk)))
  (define-values (returned-val new-actormap2 new-msgs)
    (actormap-turn* (make-transactormap new-actormap) actor-refr '()))
  (values returned-val new-actormap2 new-msgs))

;; non-committal version of actormap-run
(define (actormap-run actormap thunk)
  (define-values (returned-val _am _nm)
    (actormap-run* (make-transactormap actormap) thunk))
  returned-val)

;; committal version
;; Run, and also commit the results of, the code in the thunk
(define* (actormap-run! actormap thunk
                        #:key [reckless? #f])
  (define actor-refr
    (actormap-spawn! actormap
                     (lambda (bcom)
                       (lambda ()
                         (call-with-values thunk list)))))
  (define actormap-poker!
    (if reckless?
        actormap-reckless-poke!
        actormap-poke!))
  (apply values (actormap-poker! actormap actor-refr)))



;; Test area
;; ---------

#;(define (_test)
  (define am (make-whactormap))
  (define (^greeter _bcom my-name)
    (lambda (your-name)
      (format #f "Hello ~a, my name is ~a!" your-name my-name)))
  (define alice
    (actormap-spawn! am ^greeter "Alice"))
  (display (actormap-peek am alice "Bob"))(newline))



;; Vats
;; ----

;;;                .=======================.
;;;                |Internal Vat Schematics|
;;;                '======================='
;;;  
;;;             stack           heap
;;;              ($)         (actormap)
;;;           .-------.----------------------. -.
;;;           |       |                      |  |
;;;           |       |   .-.                |  |
;;;           |       |  (obj)         .-.   |  |
;;;           |       |   '-'         (obj)  |  |
;;;           |  __   |                '-'   |  |
;;;           | |__>* |          .-.         |  |- actormap
;;;           |  __   |         (obj)        |  |  territory
;;;           | |__>* |          '-'         |  |
;;;           |  __   |                      |  |
;;;           | |__>* |                      |  |
;;;           :-------'----------------------: -'
;;;     queue |  __    __    __              | -.
;;;      (<-) | |__>* |__>* |__>*            |  |- event loop
;;;           '------------------------------' -'  territory
;;;
;;;
;;; Finished reading core.rkt and thought "gosh what I want more out of
;;; life is more ascii art diagrams"?  Well this one is pretty much figure
;;; 14.2 from Mark S. Miller's dissertation (with some Goblins specific
;;; modifications):
;;;   http://www.erights.org/talks/thesis/
;;;
;;; If we just look at the top of the diagram, we can look at the world as
;;; it exists purely in terms of actormaps.  The right side is the actormap
;;; itself, effectively as a "heap" of object references mapped to object
;;; behavior (not unlike how in memory-unsafe languages pointers map into
;;; areas of memory).  The left-hand side is the execution of a
;;; turn-in-progress... the bottom stubby arrow corresponds to the initial
;;; invocation against some actor in the actormap, and stacked on top are
;;; calls to other actors via immediate call-return behavior using $.
;;;
;;; Vats come in when we add the bottom half of the diagram: the event
;;; loop!  An event loop manages a queue of messages that are to be handled
;;; asynchronously... one after another after another.  Each message which
;;; is handled gets pushed onto the upper-left hand stack, executes, and
;;; bottoms out in some result (which the vat then uses to resolve any
;;; promise that is waiting on this message).  During its execution, this
;;; might result in building up more messages by calls sent to <-, which,
;;; if to refrs in the same vat, will be put on the queue (FIFO order), but
;;; if they are in another vat will be sent there using the reference's vat
;;; or machine connector (depending on if local/remote).
;;;
;;; Anyway, you could implement a vat-like event loop yourself, but this
;;; module implements the general behavior.  The most important thing if
;;; you do so is to resolve promises based on turn result and also
;;; implement the vat-connnector behavior (currently the handle-message
;;; and vat-id methods, though it's not unlikely this module will get
;;; out of date... oops)
