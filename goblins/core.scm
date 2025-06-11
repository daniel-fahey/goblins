;;; Copyright 2019-2023 Christine Lemmer-Webber
;;; Copyright 2023 David Thompson
;;; Copyright 2022-2025 Jessica Tallon
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

(define-module (goblins core)
  #:export (make-actormap
            make-transactormap
            make-whactormap

            actormap-spawn
            actormap-spawn!
            ;; actormap-spawn-mactor!

            &actormap-turn-error
            actormap-turn-error-stack

            actormap-turn*
            actormap-turn

            actormap-turn-message

            actormap-peek
            actormap-poke!
            actormap-reckless-poke!

            actormap-run
            actormap-run!
            actormap-run*

            actormap-churn
            actormap-churn-run
            actormap-churn-run!

            dispatch-message
            dispatch-messages

            transactormap-reparent
            transactormap-merge!
            transactormap-buffer-merge!

            copy-whactormap

            near-refr?
            far-refr?

            spawn spawn-named
            $ <-np <-
            on
            on-sever

            <-np-extern
            listen-to

            await await*
            <<-

            spawn-promise-and-resolver

            make-actormap-read-portrait!
            actormap-take-portrait-with-read-portrait
            actormap-take-portrait
            actormap-replace-behavior
            actormap-replace-behavior!
            actormap-restore!
            actormap-restore-with-far-refrs!
            actormap-restore-from-store!
            actormap-save-to-store!

            ;; TODO: separate this out!
            <message>
            make-message message?
            message-from-vat
            message-to
            message-resolve-me
            message-args

            <questioned>
            questioned?
            questioned-message
            questioned-answer-this-question

            <listen-request>
            make-listen-request listen-request?
            listen-request-from-vat
            listen-request-to
            listen-request-listener
            listen-request-wants-partial?

            forward-to-captp?
            forward-to-captp-msg

            message-or-request-from-vat
            message-or-request-to
            message-who-wants-response

            syscaller-free
            has-syscaller?

            near-promise-broken?
            near-promise-settled?
            near-settled-promise-value
            near-promise-resolved?
            near-resolved-promise-value

            make-persistence-env
            persistence-env-compose
            namespace-env

            local-refr->persistable-object-identifier
            has-persistable-object-identifier?

            ;; Deprecated
            spawn-promise-cons
            spawn-promise-values)

  #:re-export (live-refr?
               local-refr?
               remote-refr?
               promise-refr?
               local-object-refr?
               local-promise-refr?
               remote-object-refr?
               remote-promise-refr?

               actormap-vat-connector

               whactormap?

               portraitize
               versioned)
  #:replace (spawn)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 q)
  #:use-module (rnrs bytevectors)
  #:use-module (goblins core-types)
  #:use-module (goblins abstract-types)
  #:use-module (goblins utils ghash)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins utils error-handling)
  #:use-module (goblins utils simple-sealers))


;;; Utilities (which should be moved to their own modules)
;;; ======================================================

;;; Here's basically your pre-goblins area.

;; mimic Racket's seteq
(define (vseteq . items)
  (alist->vhash (map (lambda (x) (cons x #t)) items) hashq))
(define (vseteq-add vseteq item)
  (vhash-consq item #t vseteq))
(define (vseteq-member? vseteq item)
  (vhash-assq item vseteq))

(define (persistence-env-ref env name)
  "Finds the object specification within a given persistence environment tree by the provided name"
  (hash-ref (persistence-env-name->object-spec env) name))

(define (persistence-env-ref-by-constructor env constructor)
  (hashq-ref (persistence-env-constructor->object-spec env) constructor))


;;;                  .============================.
;;;                  | High level view of Goblins |
;;;                  '============================'
;;;
;;; There's a lot of architecture here.
;;; It's a lot to take in, so let's start out with a "high level view".
;;; Here's an image to get started:
;;;
;;;   .----------------------------------.         .-------------------.
;;;   |              Node 1              |         |       Node 2      |
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
;;;  - Zooming out the farthest is the "node/network level".
;;;    There are two nodes (Node 1 and Node 2) connected over a
;;;    Goblins CapTP network.  The stubby shapes on the borders between the
;;;    nodes represent the directions of references Node 1 has to
;;;    objects in Node 2 (at the top) and references Node 2 has to
;;;    Node 1.  Both nodes in this diagram are cooperating to preserve
;;;    that Bob has access to Carol but that Carol does not have access to
;;;    Bob, and that Carlos has access to Bob but Bob does not have access
;;;    to Carlos.  (However there is no strict guarantee from either
;;;    node's perspective that this is the case... generally it's in
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
;;;   (node (vat (actormap {refr: (mactor object-handler)})))
;;;
;;; However, we could really benefit from looking at those in more detail,
;;; so from the outermost layer in...
;;;
;;;    .--- A node in Goblins is basically an OS process.
;;;    |    However, the broader Goblins CapTP/MachineTP network is
;;;    |    made up of many nodes.  A connection to another node
;;;    |    is the closest amount of "assurance" a Goblins node has
;;;    |    that it is delivering to a specific destination.
;;;    |    Nonetheless, Goblins users generally operate at the object
;;;    |    reference level of abstraction, even across nodes.
;;;    |
;;;    |    An object reference on the same node is considered
;;;    |    "local" and an object reference on another node is
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
;;; (node (vat (actormap {refr: (mactor object-handler)})))
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
;;; process runtime or between nodes across captp sessions) and
;;; offline-storeable references (sturdy refrs, a kind of bearer URI,
;;; and certificate chains, which are like "deeds" indicating that the
;;; possessor of some cryptographic material is permitted access).
;;;
;;; All offline-storeable references must first be converted to live
;;; references before they can be used (authority to do this itself a
;;; capability, as well as authority to produce these offline-storeable
;;; objects).
;;;
;;; Live references subdivide into local (on the same node) and
;;; remote (on a foreign node).  These are typed as either
;;; representing an object or a promise.
;;;
;;; (Local references also further subdivide into "near" and "far",
;;; but rather than being encoded in the reference type this is
;;; determined relative to another local-refr or the current actor
;;; context.)


;; Actormaps, etc
;; ==============






;; Weak-hash actormaps
;; ===================


(define* (make-whactormap #:key [vat-connector #f]
                          [aurie-counter 0])
  "Create and return a reference to a weak-hash actormap. If provided,
VAT-CONNECTOR is the syscaller of the containing vat.

Type: (Optional Syscaller) -> WHActormap"
  (_make-actormap whactormap-metatype
                  (make-whactormap-data (make-weak-key-hash-table))
                  vat-connector aurie-counter))


;; TODO: again, confusing (see <actormap>)
(define make-actormap make-whactormap)

(define (copy-whactormap am)
  "Copy whactormap AM to a new whactormap with the same contents."
  (define old-ht (whactormap-data-wht (actormap-data am)))
  (define new-ht (make-weak-key-hash-table))
  ;; Update new-ht with all of old-ht's values
  (hash-for-each (lambda (key val)
                   (hashq-set! new-ht key val))
                 old-ht)
  ;; Return newly made whactormap
  (_make-actormap whactormap-metatype
                  (make-whactormap-data new-ht)
                  (actormap-vat-connector am)
                  (actormap-aurie-counter am)))




(define (transactormap-ref transactormap key)
  (define tm-data (actormap-data transactormap))
  (define tm-delta
    (transactormap-data-delta tm-data))
  (define tm-val (hashq-ref tm-delta key #f))
  (when (transactormap-data-merged? tm-data)
    (error "Can't use transactormap-ref on merged transactormap"))
  (if tm-val
      ;; we got it, it's in our delta
      tm-val
      ;; search parents for key
      (let ([parent (transactormap-data-parent tm-data)])
        (actormap-ref parent key))))

(define (transactormap-set! transactormap key val)
  (define tm-delta (transactormap-data-delta (actormap-data transactormap)))
  (when (transactormap-merged? transactormap)
    (error "Can't use transactormap-set! on merged transactormap"))
  (hashq-set! tm-delta key val)
  *unspecified*)

;; Not threadsafe, but probably doesn't matter
(define (transactormap-merge! transactormap)
  "Commit the changes in TRANSACTORMAP to the generational history.

Type: TransActormap -> Void"
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
       [(eq? parent-mtype transactormap-metatype)
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
      (merge-actormap-aurie-counters! root-actormap transactormap)
      (set-transactormap-data-merged?! tm-data #t))

    root-actormap)
  (do-merge! transactormap)
  *unspecified*)

(define (transactormap? am)
  (and (actormap? am)
       (eq? (actormap-metatype am) transactormap-metatype)))

(define (transactormap-buffer-merge! transactormap)
  "Merge TRANSACTORMAP against its parent buffer (also a
transactormap).

Type: TransActormap -> Void"
  (define tm-data (actormap-data transactormap))
  (define parent (transactormap-data-parent tm-data))
  (define parent-mtype (actormap-metatype parent))
  (unless (eq? parent-mtype transactormap-metatype)
    (error "Can only do a buffered merge against another transactormap"))
  (when (or (transactormap-data-merged? tm-data)
            (transactormap-data-merged? (actormap-data parent)))
    (error "Transactormap already merged!"))
  (hash-for-each
   (lambda (key val)
     (transactormap-set! parent key val))
   (transactormap-data-delta tm-data))
  (merge-actormap-aurie-counters! parent transactormap)
  (set-transactormap-data-merged?! tm-data #t))

(define (transactormap-for-each proc transactormap)
  (define tm-data (actormap-data transactormap))
  (define tm-delta
    (transactormap-data-delta tm-data))
  (if (transactormap-data-merged? tm-data)
    (error "Can't use transactormap-for-each on merged transactormap")
    (hash-for-each proc tm-delta)))

(define transactormap-metatype
  (make-actormap-metatype 'transactormap transactormap-ref transactormap-set!
                          transactormap-for-each))

(define (make-transactormap parent)
  "Create a return a reference to a transactional actormap
representing the generation after PARENT.

Type: Actormap -> TransActormap"
  (define vat-connector (actormap-vat-connector parent))
  (_make-actormap transactormap-metatype
                  (make-transactormap-data parent (make-hash-table) #f)
                  vat-connector
                  (actormap-aurie-counter parent)))

(define (transactormap-reparent transactormap new-parent)
  (define vat-connector (actormap-vat-connector new-parent))
  (define delta (transactormap-data-delta (actormap-data transactormap)))
  (_make-actormap transactormap-metatype
                  (make-transactormap-data new-parent delta #f)
                  vat-connector
                  (actormap-aurie-counter transactormap)))


;; "Become" sealer/unsealers
;; =========================

(define-record-type <bcom-sealed>
  (make-bcom-sealed secret new-behavior return-val)
  _bcom-sealed?
  (secret bcom-sealed-secret)
  (new-behavior bcom-sealed-new-behavior)
  (return-val bcom-sealed-return-val))

(define (make-become-sealer-triplet)
  (define secret (cons '*secret* '*id*))
  (define* (bcom new-behavior #:optional [return-val *unspecified*])
    (make-bcom-sealed secret new-behavior return-val))
  (define (bcom-sealed? maybe-sealed)
    (and (_bcom-sealed? maybe-sealed)
         (eq? (bcom-sealed-secret maybe-sealed) secret)))
  (define (bcom-unseal obj)
    (unless (bcom-sealed? obj)
      (if (_bcom-sealed? obj)
          (error "Wrong bcom-unsealer for object:" obj)
          (error "Not a bcom-sealed object:" obj)))
    (values (bcom-sealed-new-behavior obj)
            (bcom-sealed-return-val obj)))
  (values bcom bcom-unseal bcom-sealed?))



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
;;;                 .----------------->.        [object]
;;;                 |                  |
;;;                 |    .--.          |    .-->[local-link]
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
;;; (remote-refrs of course never correspond to a mactor on this node;
;;; those are managed by captp.)
;;;
;;; See also:
;;;  - The comments above each of these below
;;;  - "Miranda methods":
;;;      http://www.erights.org/elang/blocks/miranda.html
;;;  - "Reference mechanics":
;;;      http://erights.org/elib/concurrency/refmech.html

;; TODO: Maybe move this to core-types?
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

;; Link to an object on the same node.
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

(define (mactor:unresolved-listeners mactor)
  (define unresolved (mactor-get-m~unresolved mactor))
  (m~unresolved-listeners unresolved))

(define (mactor:unresolved-add-listener mactor new-listener wants-partial?)
  (define new-listener-info
    (make-listener-info new-listener wants-partial?))
  (define old-unresolved (mactor-get-m~unresolved mactor))
  (define new-unresolved
    (make-m~unresolved (m~unresolved-eventual old-unresolved)
                       (cons new-listener-info
                             (m~unresolved-listeners old-unresolved))))
  (match mactor
    [(? mactor:naive?)
     (make-mactor:naive new-unresolved
                        (mactor:naive-waiting-messages mactor))]
    [(? mactor:question?)
     (make-mactor:question new-unresolved
                           (mactor:question-captp-connector mactor)
                           (mactor:question-question-finder mactor))]
    [(? mactor:closer?)
     (make-mactor:closer new-unresolved
                         (mactor:closer-point-to mactor)
                         (mactor:closer-history mactor)
                         (mactor:closer-waiting-messages mactor))]))

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

(define (near-refr? obj)
  "Return #t if OBJ is an object or promise reference within the same
vat, else #f.

Type: Any -> Boolean"
  (and (local-refr? obj)
       (let ((sys (get-syscaller-or-die)))
         (syscaller-near-refr? sys obj))))

(define (far-refr? obj)
  "Return #t if OBJ is an object or promise reference within a
different vat, else #f.

Type: Any -> Boolean"
  (and (live-refr? obj)
       (not (near-refr? obj))))

(define (near-promise-broken? promise-refr)
  (mactor:broken? (near-mactor promise-refr)))

(define* (near-promise-settled? promise-refr #:key [broken-ok? #t])
  (match (near-mactor promise-refr)
    [(or (? mactor:local-link?) (? mactor:encased?))
     #t]
    [(? mactor:broken?)
     broken-ok?]
    [_ #f]))

(define (near-settled-promise-value promise-refr)
  (define mactor (near-mactor promise-refr))
  (match mactor
    [(? mactor:local-link?)
     (mactor:local-link-point-to mactor)]
    [(? mactor:encased?)
     (mactor:encased-val mactor)]
    [(? mactor:broken?)
     (raise-exception (mactor:broken-problem mactor))]))

(define* (near-promise-resolved? promise-refr #:key [broken-ok? #t])
  (match (near-mactor promise-refr)
    [(or (? mactor:local-link?) (? mactor:encased?))
     #t]
    [(? mactor:broken?)
     broken-ok?]
    [_ #f]))

(define (near-resolved-promise-value promise-refr)
  (define mactor (near-mactor promise-refr))
  (match mactor
    [(? mactor:local-link?)
     (mactor:local-link-point-to mactor)]
    [(? mactor:remote-link?)
     (mactor:remote-link-point-to mactor)]
    [(? mactor:encased?)
     (mactor:encased-val mactor)]
    [(? mactor:broken?)
     (raise-exception (mactor:broken-problem mactor))]))

;; Dangerous and dynamic... not intended to be exposed outside of here
;; at this time, anyway.
;; Used to implement some promise-introspection methods...
(define (near-mactor refr)
  (syscaller-near-mactor (current-syscaller) refr))



;; Messages
;; --------

;; These are the main things that get sent as the toplevel of a turn in a vat!
(define-record-type <message>
  (make-message from-vat to resolve-me args)
  message?
  ;; which vat connector the message came from
  (from-vat message-from-vat)
  ;; who's receiving the message (the invoked actor)
  (to message-to)
  ;; who's interested in the result (a resolver)
  (resolve-me message-resolve-me)
  ;; arguments to the invoked actor
  (args message-args))

;; When speaking to the captp connector, sometimes we're really asking
;; a question.
(define-record-type <questioned>
  (make-questioned message answer-this-question)
  questioned?
  (message questioned-message)
  ;; This one's a question-finder... supplied by the captp connector!
  (answer-this-question questioned-answer-this-question))

;; Sent in the same way as <message>, but does listen requests specifically
(define-record-type <listen-request>
  (make-listen-request from-vat to listener wants-partial?)
  listen-request?
  (from-vat listen-request-from-vat)
  (to listen-request-to)
  (listener listen-request-listener)
  (wants-partial? listen-request-wants-partial?))

;; This kluge is for when we need to forward a message to captp... but
;; typically also it might have a question-finder for the `to' field...
;; so we put in this hack to let the code handling the turn/churn know
;; how to dispatch these since the message might not be addressed to
;; a normal refr.  This is kind of weird though, because you don't need
;; this if we have a remote-refr that already has a captp-connector.
;; It could be that instead we should make another kind of remote-refr
;; specifically for questions which have not been assigned slots... yet.
(define-record-type <forward-to-captp>
  (make-forward-to-captp msg connector)
  forward-to-captp?
  (msg forward-to-captp-msg)
  (connector forward-to-captp-connector))

(define message-or-request-from-vat
  (match-lambda
    [(? forward-to-captp? forward-me)
     (message-or-request-from-vat (forward-to-captp-msg forward-me))]
    [(? message? msg) (message-from-vat msg)]
    [(? listen-request? lr) (listen-request-from-vat lr)]
    [(? questioned? qstn) (message-from-vat (questioned-message qstn))]))

(define message-or-request-to
  (match-lambda
    [(? forward-to-captp? forward-me)
     (message-or-request-to (forward-to-captp-msg forward-me))]
    [(? message? msg) (message-to msg)]
    [(? listen-request? lr) (listen-request-to lr)]
    [(? questioned? qstn) (message-to (questioned-message qstn))]))

(define message-who-wants-response
  (match-lambda
    [(? message? msg)
     (message-resolve-me msg)]
    [(? listen-request? lr)
     (listen-request-listener lr)]
    [(? questioned? qm)
     (message-who-wants-response (questioned-message qm))]))


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

;; Note that we've commented this all out in the meanwhile.
;; Instead we are taking two opposing perspectives: the Racket world takes
;; the MarkM "very cautious about coroutines" perspective and the Guile world
;; makes no protections against re-entrancy...

;; (define %re-entry-protect (make-parameter #f))
;;
;; (define (_re-protec proc)
;;   (define result
;;     ;; protect against re-entrancy attacks
;;     ;; (... but also "protects" against live debugging, unfortunately)
;;     (with-continuation-barrier
;;      (lambda ()
;;        (with-exception-handler
;;            (lambda (exn)
;;              (list 'error exn))
;;          (lambda ()
;;            ;; actors are only permitted one value from their
;;            ;; continuation
;;            (define result
;;              (proc))
;;            (list 'success result))
;;          #:unwind? #t
;;          #:unwind-for-type #t))))
;;   (match result
;;     [('success result)
;;      result]
;;     [('error err)
;;      (raise-exception err)]))
;;
;; (define (with-re-entry-protection proc)
;;   (if (%re-entry-protect)
;;       (_re-protec proc)
;;       (proc)))



;; Syscaller
;; =========

;; Do NOT export this esp under serious ocap confinement
(define current-syscaller (make-parameter #f))

(define-record-type <syscaller>
  (_make-syscaller actormap vat-connector new-msgs closed?)
  syscaller?
  ;; The actormap this syscaller is operating upon
  (actormap syscaller-actormap)
  ;; This is, effectively, cached from pulling off the actormap
  ;; for efficiency purposes
  (vat-connector syscaller-vat-connector)
  ;; New messages are queued up in a simple linked list
  (new-msgs syscaller-new-msgs set-syscaller-new-msgs!)
  ;; Whether we have finished a turn (or whatever) on this actormap
  ;; and further operations on it should be disallowed
  (closed? syscaller-closed? set-syscaller-closed?!))

(define (make-syscaller actormap)
  "Construct a new syscaller which will operate on ACTORMAP"
  (_make-syscaller actormap (actormap-vat-connector actormap) '() #f))

(define (syscaller-near-refr? syscaller refr)
  "Checks if OBJ is a local refr of SYSCALLER's actormap"
  (and (local-refr? refr)
       (eq? (local-refr-vat-connector refr)
            (syscaller-vat-connector syscaller))))

;; TODO: Rename to syscaller-actormap-ref ?
(define (syscaller-near-mactor syscaller refr)
  "Selects the mactor for OBJ refr associated with actormap of SYSCALLER"
  (actormap-ref (syscaller-actormap syscaller) refr))

(define (syscaller-spawn-mactor syscaller mactor debug-name)
  (actormap-spawn-mactor! (syscaller-actormap syscaller) mactor debug-name))

;; Helper procedure for following syscaller operations
(define-inlinable (actormap-ref-or-die actormap to-refr)
  "Extract TO-REFR from SYSCALLER's actormap, otherwise fail"
  (define mactor
    (actormap-ref actormap to-refr))
  (unless mactor
    (error 'no-such-actor "no actor with this id in this vat:" to-refr))
  mactor)

;; call actor's behavior
(define (syscaller-$ syscaller to-refr args)
  (define actormap
    (syscaller-actormap syscaller))
  (define vat-connector
    (syscaller-vat-connector syscaller))

  ;; Restrict to live-refrs which appear to have the same
  ;; vat-connector as us

  ;; TODO: Are either of the following two checks required?
  ;;   Consider when optimization testing whether removing these
  ;;   is worthwhile or they are redundant by actormap-ref-or-die.

  (unless (local-refr? to-refr)
    (error 'not-callable
           "Not a live reference:" to-refr))

  (unless (eq? (local-refr-vat-connector to-refr)
               vat-connector)
    (error 'not-callable
           "Not in the same vat:" to-refr))

  (define mactor
    (actormap-ref-or-die actormap to-refr))

  (match mactor
    [(? mactor:object?)
     (let ((actor-behavior
            (mactor:object-behavior mactor))
           (become?
            (mactor:object-become? mactor))
           (become-unsealer
            (mactor:object-become-unsealer mactor)))
       (define (_do-actor-call)
         (apply actor-behavior args))
       (define (_handle-await k fulfill-proc promise?)
         (define-values (waiting-promise waiting-resolver)
           (_spawn-promise-and-resolver))
         ;; Let the fulfill-proc set up how we resolve this
         ;; (see the `await' procedure for an example)
         (fulfill-proc waiting-resolver)
         ;; We wait on the coroutine to see if it succeds or not,
         ;; and re-awaken to the continuation set up by `await*'
         ;; which will act appropriately depending on whether
         ;; we tell it this succeeds or fails.
         ;; Note that the `bcom' relevant to this actor will no longer
         ;; work as a form of "become"... a feature, actually!
         ;;
         ;; However, it could be that this is really
         ;; the right thing to do because promise chains for an
         ;; infinite loop could themselves become infinite.  So
         ;; maybe this is a feature.
         (define maybe-on-vow
           (on waiting-promise
               (lambda (val)
                 (call-with-prompt *actor-await-prompt*
                   (lambda ()
                     (k 'resume val))
                   _handle-await))
               #:catch
               (lambda (err)
                 (call-with-prompt *actor-await-prompt*
                   (lambda ()
                     (k 'error err))
                   _handle-await))
               #:promise? promise?))
         ;; Since we do not allow for "returning useful values" in
         ;; case of coroutines, we default to returning the symbol `*awaited*'.
         ;; However, users can specifically select for a promise to be returned
         ;; by passing in #:promise? #t.
         (if promise?
             maybe-on-vow
             '*awaited*))

       ;; I guess watching for this guarantees that an immediate call
       ;; against a local actor will not be tail recursive.
       ;; TODO: We need to document that.
       (define-values (new-behavior return-val self-portrait)
         (let ([returned
                (call-with-prompt *actor-await-prompt*
                  _do-actor-call _handle-await)])
           (if (become? returned)
               ;; The unsealer unseals both the behavior and return-value anyway
               (let-values ([(new-beh return-val) (become-unsealer returned)])
                 (match new-beh
                   [(? portraitized-behavior? pb)
                    (values (portraitized-behavior-behavior pb)
                            return-val
                            (portraitized-behavior-self-portrait pb))]
                   [_ (values new-beh return-val (mactor:object-self-portrait mactor))]))

               ;; In this case, we're not becoming anything, so just give us
               ;; the return-val
               (values #f returned (mactor:object-self-portrait mactor)))))

       ;; if a new behavior for this actor was specified,
       ;; let's replace it
       (when new-behavior
         (unless (procedure? new-behavior)
           (error 'become-failure "Tried to become a non-procedure behavior:"
                  new-behavior))
         (actormap-set! actormap to-refr
                        (make-mactor:object
                         new-behavior
                         (mactor:object-constructor-refr mactor)
                         (mactor:object-spawned-constructor mactor)
                         self-portrait
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
     (syscaller-$ syscaller (mactor:local-link-point-to mactor)
                  args)]
    ;; Not a callable mactor!
    [_other
     (error 'not-callable
            "Not callable with $ or from toplevel <-:"
            'to-refr: to-refr 'args: args
            'mactor: mactor)]))

;; spawn a new actor
(define (syscaller-spawn syscaller maybe-constructor args debug-name)
  (define vat-connector (syscaller-vat-connector syscaller))
  (define actormap (syscaller-actormap syscaller))
  (define-values (become become-unsealer become-sealed?)
    (make-become-sealer-triplet))
  (define-values (constructor constructor-refr)
    (values (if (redefinable-object? maybe-constructor)
                (redefinable-object-constructor maybe-constructor)
                maybe-constructor)
            maybe-constructor))
  (define initial-behavior
    (apply constructor become args))
  (define* (create-refr beh #:optional maybe-self-portrait)
    (match beh
      [(? portraitized-behavior?)
       (create-refr (portraitized-behavior-behavior beh)
                    (portraitized-behavior-self-portrait beh))]
      ;; New procedure, so let's set it
      [(? procedure?)
       (let ((actor-refr
              (make-local-object-refr debug-name vat-connector
                                      (increment-actormap-aurie-counter! actormap))))
         (actormap-set! actormap actor-refr
                        (make-mactor:object beh
                                            constructor-refr
                                            constructor
                                            maybe-self-portrait
                                            become-unsealer become-sealed?))
         actor-refr)]
      ;; If someone returns another actor, just let that be the actor
      [(? live-refr? pre-existing-refr)
       pre-existing-refr]
      [_
       (error 'invalid-actor-handler "Not a procedure or live refr:" initial-behavior)]))
  (create-refr initial-behavior))

(define (syscaller-fulfill-promise syscaller promise-id sealed-val)
  (define actormap (syscaller-actormap syscaller))

  (call/ec
   (lambda (return-early)
     (define orig-mactor
       (actormap-ref-or-die actormap promise-id))
     (unless (mactor:unresolved? orig-mactor)
       (error 'resolving-resolved
              "Attempt to resolve resolved actor:" promise-id))
     (define resolve-to-val
       (unseal-mactor-resolution orig-mactor sealed-val))

     (define orig-waiting-messages
       (match orig-mactor
         [(? mactor:naive?)
          (mactor:naive-waiting-messages orig-mactor)]
         [(? mactor:closer?)
          (mactor:closer-waiting-messages orig-mactor)]
         [_ '()]))

     (define (forward-messages)
       (let send-rest ([waiting-messages orig-waiting-messages])
         (match waiting-messages
           ['() *unspecified*]
           ;; TODO: add support for <questioned> here, right?!?!
           [((? message? msg) rest-waiting ...)
            (let ((resolve-me (message-resolve-me msg))
                  (args (message-args msg)))
              ;; preserve FIFO by recursing first
              (send-rest rest-waiting)
              ;; and then send this message along
              (syscaller-send-message syscaller resolve-to-val resolve-me args))])))

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
             (syscaller-break-promise syscaller promise-id
                                      ;; TODO: we need some sort of error type we do
                                      ;;   allow to explicitly be shared, this one is a
                                      ;;   reasonable candidate
                                      'cycle-in-promise-resolution)))
          (make-mactor:local-link resolve-to-val)]
         [(? remote-object-refr?)
          ;; Since the captp connection is the one that might break this,
          ;; we need to ask it what it uses as its resolver unsealer/tm
          ;; @@: ... This doesn't seem like a good solution.
          ;;   Maybe bears re-examination with the addition of on-sever.
          (let* ([connector (remote-refr-captp-connector resolve-to-val)]
                 [partition-unsealer-tm-cons (connector 'partition-unsealer-tm-cons)])
            ;; TODO: Do we need to notify it that we want to know about
            ;;   breakage?  Presumably... so do it here instead...?
            ;; TODO: Do we really need to pattern match against a cons here?
            ;;   Couldn't we return multiple values?
            (match partition-unsealer-tm-cons
              [(new-resolver-unsealer . new-resolver-tm?)
               (make-mactor:remote-link (make-m~eventual new-resolver-unsealer
                                                         new-resolver-tm?)
                                        resolve-to-val)]))]
         [(or (? local-promise-refr?)
              (? remote-promise-refr?))
          (define new-history
            (if (mactor:closer? orig-mactor)
                (vseteq-add (mactor:closer-history orig-mactor)
                            (mactor:closer-point-to orig-mactor))
                (vseteq promise-id)))
          ;; Detect cycles!
          (when (vseteq-member? new-history resolve-to-val)
            ;; not sure we actually need to return anything, but I guess
            ;; this is mildly future-proof.
            (return-early
             ;; We want to break this because it should be explicitly clear
             ;; to everyone that the promise was broken.
               (syscaller-break-promise syscaller promise-id
                                        ;; TODO: we need some sort of error type we do
                                        ;;   allow to explicitly be shared, this one is a
                                        ;;   reasonable candidate
                                        'cycle-in-promise-resolution)))

          ;; Make a new set of resolver sealers for this.
          ;; However, we don't use the general ^resolver because we're
          ;; explicitly using the fulfilled-handler/broken-handler things
          (let*-values ([(new-resolver-sealer new-resolver-unsealer new-resolver-tm?)
                         (make-sealer-triplet 'fulfill-promise)]
                        [(new-resolver)
                         (syscaller-spawn syscaller ^resolver
                                          (list promise-id new-resolver-sealer)
                                          '^resolver)])
            ;; Now subscribe to the promise...
            (syscaller-send-listen syscaller resolve-to-val new-resolver #t)
            (let* ([new-listeners
                    ;; inform those who want partial resolution and gather those who don't
                    (let lp ([listeners orig-listeners]
                             [new-listeners '()])
                      (match listeners
                        ['() new-listeners]
                        [(listener-info rest-listeners ...)
                         (if (listener-info-wants-partial? listener-info)
                             ;; resolve and drop out of listeners
                             (begin
                               ;; resolve
                               (syscaller-<-np syscaller
                                               (listener-info-resolve-me listener-info)
                                               (list 'fulfill resolve-to-val))
                               ;; recurse and drop out
                               (lp rest-listeners new-listeners))
                             ;; recurse with this one present
                             (lp rest-listeners
                                 (cons listener-info new-listeners)))]))]
                   [new-eventual (make-m~eventual new-resolver-unsealer
                                                  new-resolver-tm?)]
                   [new-unresolved (make-m~unresolved new-eventual
                                                      new-listeners)])
              ;; Now we become "closer" to this promise
              (make-mactor:closer new-unresolved
                                  resolve-to-val new-history
                                  new-waiting-messages)))]
         ;; anything else is an encased value
         [_ (make-mactor:encased resolve-to-val)]))

     ;;  - Now actually switch to the new mactor state
     (actormap-set! actormap promise-id
                    next-mactor-state)

     ;; Resolve listeners, if appropriate (ie, if not mactor:closer)
     (unless (mactor:unresolved? next-mactor-state)
       (for-each (lambda (listener-info)
                   (syscaller-<-np syscaller
                                   (listener-info-resolve-me listener-info)
                                   (list 'fulfill resolve-to-val)))
                 orig-listeners)))))

;; TODO: Add support for broken-because-of-network-partition support
;;   even for mactor:remote-link
(define (syscaller-break-promise syscaller promise-id sealed-problem)
  (define actormap (syscaller-actormap syscaller))

  (match (actormap-ref actormap promise-id)
    ;; TODO: Not just local-promise, anything that can
    ;;   break
    [(? mactor:unresolved? unresolved-mactor)
     (define problem
       (unseal-mactor-resolution unresolved-mactor sealed-problem))
     (define unresolved-listeners
       (mactor:unresolved-listeners unresolved-mactor))
     (define waiting-messages
       (match unresolved-mactor
         [(? mactor:naive?)
          (mactor:naive-waiting-messages unresolved-mactor)]
         [(? mactor:closer?)
          (mactor:closer-waiting-messages unresolved-mactor)]
         [_ '()]))
     ;; Combine together the unresolved-listeners with the resolvers
     ;; of waiting-messages.
     (define all-interested-listeners
       (append (map message-resolve-me waiting-messages)
               (map listener-info-resolve-me unresolved-listeners)))
     ;; Inform all listeners of the resolution
     (for-each (lambda (listener)
                   (syscaller-<-np syscaller listener (list 'break problem)))
               all-interested-listeners)
     ;; Now we "become" broken with that problem
     (actormap-set! actormap promise-id
                    (make-mactor:broken problem))]
    [(? mactor:remote-link?)
     (error "TODO: Implement breaking on captp disconnect!")]
    [#f (error "no actor with this id")]
    [_ (error "can only resolve eventual references")]))

;; Note that syscaller-handle-message is really, seriously for handling *toplevel*
;; messages... ie, turns.
;; This is the bulk of what's called and handled by actormap-turn-message.
;; (As opposed to actormap-turn*, which only supports calling, this also
;; handles any toplevel invocation of an actor, probably via message send.)
(define (syscaller-handle-message syscaller msg)
  (define actormap (syscaller-actormap syscaller))
  (define vat-connector (syscaller-vat-connector syscaller))

  (define to-refr (message-to msg))
  (define resolve-me (message-resolve-me msg))
  (define args (message-args msg))

  (unless (near-refr? to-refr)
    (error 'not-a-near-refr "Not a near refr:" to-refr))

  ;; Prevent someone trying to throw this vat into an infinite loop
  (when (eq? to-refr resolve-me)
    (error 'same-recipient-and-resolver
           "Recipient and resolver are the same:" to-refr))

  (let ([call-with-resolution
         (lambda (proc)
           #;(define (handle-exn err)
           (when display-or-log-error
           (display-or-log-error err))
           ;; We need to revert any messages that were going
           ;; to send to preserve transactionality
           (set! new-msgs '())
           ;; ... but we're still going to send this one
           (when resolve-me
             (_<-np resolve-me (list 'break err)))
           `#(fail ,err))
           (define (do-call)
             (define call-result
               (proc))
             (when resolve-me
                 (syscaller-<-np syscaller resolve-me (list 'fulfill call-result)))
             call-result)
           #;(with-exception-handler handle-exn
           do-call
           #:unwind? #t
           #:unwind-for-type #t)
           (do-call))]
        [orig-mactor (actormap-ref-or-die actormap to-refr)])
    (match orig-mactor
      ;; If it's callable, we just use the call behavior, because
      ;; that's effectively the same code we'd be running anyway.
      ;; However, we do want to handle the resolution.
      [(or (? mactor:object?)
           (? mactor:encased?))
       (call-with-resolution
        (lambda () (syscaller-$ syscaller to-refr args)))]
      [(? mactor:local-link?)
       (let ((point-to (mactor:local-link-point-to orig-mactor)))
         (cond
          [(near-refr? point-to)
           (call-with-resolution
            (lambda () (syscaller-$ syscaller point-to args)))]
          ;; it's not near so we need to pass this along
          [else
           (syscaller-send-message syscaller point-to resolve-me args)
           *unspecified*]))]
      [(? mactor:broken?)
       (syscaller-<-np syscaller resolve-me (list 'break (mactor:broken-problem orig-mactor)))
       *unspecified*]
      [(? mactor:remote-link?)
       (let ([point-to (mactor:remote-link-point-to orig-mactor)])
         (call-with-resolution
          (lambda ()
            (let ((<-sysc-send (if resolve-me
                                   syscaller-<-
                                   syscaller-<-np)))
              ;; Pass along the message.
              ;; Only produce a promise if we have a resolver.
              (<-sysc-send syscaller point-to args)))))]
      ;; Messages sent to a promise that is "closer" are a kind of
      ;; intermediate state; we build a queue.
      [(? mactor:closer?)
       (match (mactor:closer-point-to orig-mactor)
         ;; If we're pointing at another near promise then we recurse
         ;; to _handle-messages with the next promise...
         [(? local-promise-refr? point-to)
          ;; Now we need to see if it's in the same vat...
          (cond
           [(near-refr? point-to)
            ;; (We don't use call-with-resolution because the next one will!)
            (syscaller-handle-message
             syscaller (make-message vat-connector point-to resolve-me args))]
           [else
            ;; Otherwise, we need to forward this message to the appropriate
            ;; vat
            (syscaller-send-message syscaller point-to resolve-me args)
            *unspecified*])]
         ;; But if it's a remote promise then we queue it in the waiting
         ;; messages because we prefer to have messages "swim as close
         ;; as possible to the CapTP barrier where possible", with
         ;; the exception of questions/answers which always cross over
         ;; (see mactor:question handling later in this procedure)
         [(? remote-promise-refr? point-to)
          (let ((unresolved (mactor:closer-unresolved orig-mactor))
                (point-to (mactor:closer-point-to orig-mactor))
                (history (mactor:closer-history orig-mactor))
                (waiting-messages (mactor:closer-waiting-messages orig-mactor)))
            ;; Since we're queueing to send the message until it resolves
            ;; we don't resolve the problem here... hence we don't
            ;; use call-with-resolution here either.
            (actormap-set! actormap to-refr
                           (make-mactor:closer
                            unresolved point-to history
                            (cons msg waiting-messages))))
          *unspecified*])]
      ;; Similar to the above w/ remote promises, except that we really
      ;; just don't know where things go *at all* yet, so no swimming
      ;; occurs.
      [(? mactor:naive?)
       (let ((unresolved (mactor-get-m~unresolved orig-mactor))
             (waiting-messages (mactor:naive-waiting-messages orig-mactor)))
         (actormap-set! actormap to-refr
                        (make-mactor:naive unresolved
                                           (cons msg waiting-messages)))
         *unspecified*)]
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
            (let*-values ([(followup-question-finder)
                           (captp-connector 'new-question-finder)]
                          [(followup-question-promise followup-question-resolver)
                           (_spawn-promise-and-resolver #:question-finder
                                                        followup-question-finder
                                                        #:captp-connector
                                                        captp-connector)])
              (let ((new-msg
                     (make-forward-to-captp
                      (make-questioned (make-message vat-connector
                                                     to-question-finder
                                                     followup-question-resolver
                                                     args)
                                       followup-question-finder)
                      captp-connector)))
                (syscaller-queue-new-msg! syscaller new-msg)
                followup-question-promise))]
           ;; Otherwise, we can just send it without any question and return
           ;; void
           [else
            (let ((new-msg
                   (make-forward-to-captp
                    (make-message vat-connector to-question-finder #f args)
                    captp-connector)))
              (syscaller-queue-new-msg! syscaller new-msg)
              *unspecified*)])))])))

;; Helper to the below two procedures
(define* (syscaller-send-message syscaller to-refr resolve-me args
                                 #:key [answer-this-question #f])
  (define vat-connector (syscaller-vat-connector syscaller))
  (unless (live-refr? to-refr)
    (error 'send-message
           "Don't know how to send a message to:" to-refr))
  (let* ((base-message (make-message vat-connector to-refr resolve-me args))
         (new-message
          (if answer-this-question
              (make-questioned base-message answer-this-question)
              base-message)))
    (syscaller-queue-new-msg! syscaller new-message)))

(define (syscaller-<-np syscaller to-refr args)
  (syscaller-send-message syscaller to-refr #f args)
  *unspecified*)

;; Well, this does do a bit more heavy lifting than *just* call
;; syscaller-send-message.
;;
;; It also constructs a promise (including, possibly, a question promise)
(define (syscaller-<- syscaller to-refr args)
  (match to-refr
    [(? local-refr?)
     (let-values ([(promise resolver)
                   (_spawn-promise-and-resolver)])
       (syscaller-send-message syscaller to-refr resolver args)
       promise)]
    [(? remote-refr?)
     (let*-values (((captp-connector)
                    (remote-refr-captp-connector to-refr))
                   ((question-finder)
                    (captp-connector 'new-question-finder))
                   ((promise resolver)
                    (_spawn-promise-and-resolver #:question-finder
                                                 question-finder
                                                 #:captp-connector
                                                 captp-connector)))
       (syscaller-send-message syscaller to-refr resolver args
                               #:answer-this-question question-finder)
       promise)]
    [to-refr
     (error 'send-message
            "Don't know how to send a message to:" to-refr)]))

(define* (syscaller-send-listen syscaller to-refr listener
                                #:optional [wants-partial? #f])
  (define vat-connector (syscaller-vat-connector syscaller))
  (match to-refr
    [(? live-refr?)
     (let ([listen-req
            (make-listen-request vat-connector to-refr listener wants-partial?)])
       (syscaller-queue-new-msg! syscaller listen-req))]
    [val (syscaller-<-np syscaller listener (list 'fulfill val))]))

(define (syscaller-handle-listen syscaller to-refr listener wants-partial?)
  #;(define (handle-exn err)
  (when display-or-log-error
  (display-or-log-error err while-handling-listen-header))
  `#(fail ,err))
  (define actormap (syscaller-actormap syscaller))

  (define (do-call)
    (unless (near-refr? to-refr)
      (error 'not-a-near-refr "Not a near refr:" to-refr))
    (define mactor
      (actormap-ref-or-die actormap to-refr))
    (match mactor
      [(? mactor:local-link?)
       (let ((point-to
              (mactor:local-link-point-to mactor)))
         (if (near-refr? point-to)
             (syscaller-handle-listen syscaller
                                      (mactor:local-link-point-to mactor)
                                      listener wants-partial?)
             (syscaller-send-listen syscaller point-to listener wants-partial?)))]
      ;; This object is a local promise, so we should handle it.
      [(? mactor:unresolved?)
       ;; Set a new version of the local-promise with this
       ;; object as a listener
       (actormap-set! actormap to-refr
                      (mactor:unresolved-add-listener mactor listener
                                                      wants-partial?))]
      ;; In the following cases we can resolve the listener immediately...
      [(? mactor:broken? mactor)
       (syscaller-<-np syscaller listener (list 'break (mactor:broken-problem mactor)))]
      [(? mactor:encased? mactor)
       (syscaller-<-np syscaller listener (list 'fulfill (mactor:encased-val mactor)))]
      [(? mactor:object? mactor)
       (syscaller-<-np syscaller listener (list 'fulfill to-refr))]
      ;; For remote links, we resolve directly to that reference
      [(? mactor:remote-link? mactor)
       (syscaller-<-np syscaller listener (list 'fulfill (mactor:remote-link-point-to mactor)))])
    *unspecified*)
  #;(with-exception-handler handle-exn
  do-call
  #:unwind? #t
  #:unwind-for-type #t)
  (do-call))

;; At THIS stage, fulfilled-handler, broken-handler, finally-handler should
;; be actors or #f.  That's not the case in the user-facing
;; `on' procedure.
(define* (syscaller-on syscaller on-refr fulfilled-handler
                       broken-handler finally-handler promise?)
  (define-values (return-promise return-p-resolver)
    (if promise?
        (spawn-promise-and-resolver)
        (values #f #f)))

  ;; These two procedures are called once the fulfillment
  ;; or break of the on-refr has actually occurred.
  (define (handle-resolution on-resolution
                             resolve-fulfill-command)
    (lambda (val)
      (cond [on-resolution
             ;; We can't use _send-message directly, because this may
             ;; be in a separate syscaller at the time it's resolved.
             (let ((syscaller (get-syscaller-or-die)))
               ;; But anyway, we want to resolve the return-p-resolver with
               ;; whatever the on-resolution is, which is why we do this goofier
               ;; roundabout
               (syscaller-send-message syscaller
                                       on-resolution
                                       ;; Which may be #f!
                                       return-p-resolver
                                       (list val))
               (when finally-handler
                 (<-np finally-handler)))]
            ;; There's no on-resolution, which means we can just fulfill
            ;; the promise immediately!
            [else
             (when finally-handler
               (<-np finally-handler))
             (when return-p-resolver
               (<-np return-p-resolver resolve-fulfill-command val))])))

  (define handle-fulfilled
    (handle-resolution fulfilled-handler 'fulfill))
  (define handle-broken
    (handle-resolution broken-handler 'break))

  ;; The purpose of this listener is that the promise
  ;; *hasn't resolved yet*.  Because of that we need to
  ;; queue something to happen *once* it resolves.
  (define (^on-listener bcom)
    (match-lambda*
      [('fulfill val)
       (handle-fulfilled val)
       *unspecified*]
      [('break problem)
       (handle-broken problem)
       *unspecified*]))
  (define listener
    (syscaller-spawn syscaller ^on-listener '() '^on-listener))
  (syscaller-send-listen syscaller on-refr listener)
  (when promise?
    return-promise))

(define (syscaller-queue-new-msg! syscaller new-msg)
  (define newer-msgs
    (cons new-msg (syscaller-new-msgs syscaller)))
  (set-syscaller-new-msgs! syscaller newer-msgs))

(define* (syscaller-set-closed! syscaller #:optional [val #t])
  (set-syscaller-closed?! syscaller val))


(define (call-with-fresh-syscaller am proc)
  (define sys (make-syscaller am))
  ;; The purpose of closing things is to detect certain kinds of errors
  ;; where the syscaller is captured and remains open post-execution.
  ;; However, it's kind of probabalistic to do this at all, since the
  ;; open/closed nature is temporal... still, this has helped identify
  ;; some bugs so it's probably worth keeping.
  ;; However, we now not only close on leaving the dynamic wind, we also
  ;; open on entering.  The reason is that suspending to the event loop
  ;; in fibers will close it, even before a turn is over (due to completion
  ;; or due to an exception).  So we need to re-open on the way back in.
  (dynamic-wind
    (lambda ()
      (set-syscaller-closed?! sys #f))
    (lambda ()
      (parameterize ([current-syscaller sys])
        (proc sys)))
    (lambda ()
      (set-syscaller-closed?! sys #t))))

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

(define (has-syscaller?)
  (syscaller? (current-syscaller)))



;; Core API (spawn, $, <-, <-np, on)
;; =================================

;; System calls
(define (spawn constructor . args)
  "Construct and return a reference to the actor described by
CONSTRUCTOR, passing it ARGS.

Type: Constructor Any ... -> Actor"
  (define sys (get-syscaller-or-die))
  (syscaller-spawn sys constructor args (procedure-name constructor)))

(define (spawn-named name constructor . args)
  "Construct and return a reference to an actor with the debug name
NAME described by CONSTRUCTOR, passing it ARGS.

Type: Symbol Constructor Any ... -> Actor"
  (define sys (get-syscaller-or-die))
  (syscaller-spawn sys constructor args name))

(define ($ refr . args)
  "Synchronously invoke REFR with ARGS; return the result.

Type: Actor Any ... -> Any"
  (define sys (get-syscaller-or-die))
  (syscaller-$ sys refr args))

(define (<- refr . args)
  "Asynchronously invoke REFR with ARGS; return a promise.

Type: Actor Any ... -> Promise"
  (define sys (get-syscaller-or-die))
  (syscaller-<- sys refr args))

(define (<-np refr . args)
  "Asynchronously invoke REFR with ARGS; return nothing.

Type: Actor Any ... -> Void"
  (define sys (get-syscaller-or-die))
  (syscaller-<-np sys refr args))

(define (<-np-extern to-refr . args)
  "Asynchronously invoke the far REFR with ARGS; return nothing.

Type: Actor Any ... -> Void"
  (match to-refr
    [(? local-refr?)
     (let ((vat-connector (local-refr-vat-connector to-refr)))
       (unless vat-connector
         (error "Can't use <-np-extern on local-refr with no vat-connector"))
       (vat-connector 'handle-message 0
                      (make-message vat-connector to-refr #f args))
       *unspecified*)]
    [(? remote-refr?)
     (let ((captp-connector (remote-refr-captp-connector to-refr)))
       (captp-connector 'handle-message
                        (make-message captp-connector to-refr #f args))
       *unspecified*)]))


;; Listen to a promise
(define* (listen-to to-refr listener #:key [wants-partial? #f])
  "Wait for TO-REFR to resolve then inform LISTENER. If WANTS-PARTIAL?
is #t, return updates rather than waiting for full promise resolution.
Return nothing.

Type: Promise Actor -> Void"
  (define sys (get-syscaller-or-die))
  (syscaller-send-listen sys to-refr listener wants-partial?))

(define* (on vow #:optional (fulfilled-handler #f)
             #:key
             [catch #f]
             [finally #f]
             [promise? #f])
  "Resolve the promise VOW, pass the result to FULFILLED-HANDLER if it
is provided, and return the result. If the procedure CATCH is
provided, it is called on the exception object of any errors. If
FINALLY is provided, it is run after FULFILLED-HANDLER and/or CATCH.
If PROMISE? is #t, the returned value is a promise.

Type: Promise (Optional (Any -> Any))
(Optional (#:catch (Exception -> Any)))
(Optional (#:finally (-> Any))) (Optional Boolean) -> (U Any Promise)"
  (define broken-handler catch)
  (define finally-handler finally)
  (define sys (get-syscaller-or-die))
  (define (maybe-actorize obj proc-name)
    (match obj
      ;; if it's a reference, it's already fine
      [(? live-refr?)
       obj]
      ;; if it's a procedure, let's spawn it
      [(? procedure?)
       (let ((already-ran
              (lambda _
                (error "Already ran for automatically generated listener"))))
         (spawn-named
          proc-name
          (lambda (bcom)
            (lambda args
              (bcom already-ran (apply obj args))))))]
      ;; If it's #f, leave it as #f
      [#f #f]
      ;; Otherwise, this doesn't belong here
      [_ (error "Invalid handler for on:" obj)]))
  (syscaller-on sys vow (maybe-actorize fulfilled-handler 'fulfilled-handler)
                (maybe-actorize broken-handler 'broken-handler)
                (maybe-actorize finally-handler 'finally-handler)
                promise?))

;; Note that this is on severance of the *connection of this reference*,
;; and if it's a promise, does not follow the promise to its resolution.
;; It will naively treat it as the connection of the *promise*.
;; If you need a more precise object, use `on` to get the fully resolved
;; object.
;;
;; The thing that gets returned is the ability to cancel interest.
(define (on-sever remote-object-refr sever-handler)
  "Register `sever-handler' when connection for `remote-object-refr' is severed"
  (define-values (sever-vow sever-resolver)
    (spawn-promise-and-resolver))
  (define captp-connector
    (remote-refr-captp-connector remote-object-refr))
  (define connector-obj
    (captp-connector 'connector-obj))
  (define connector-cancel-vow
    (<- connector-obj 'resolve-on-sever sever-resolver))

  (on sever-vow
      (match-lambda
        ['canceled *unspecified*]
        [('severed shutdown-type reason)
         (match sever-handler
           [(? procedure?)
            (sever-handler shutdown-type reason)]
           [(? live-refr?)
            (<-np sever-handler shutdown-type reason)])]))


  ;; Notifies the captp connector we're no longer interested and cancels
  ;; the handler here locally too.
  (define (^cancel-interest bcom)
    (lambda ()
      (<-np connector-obj 'cancel-sever-interest sever-resolver)
      (<-np sever-resolver 'resolve 'canceled)
      (bcom (lambda _ *unspecified*))))
  (spawn ^cancel-interest))



;; Coroutine support
;; =================

(define *actor-await-prompt* (make-prompt-tag 'await-prompt))

;; We default to `promise? #f' to avoid accidental infinite promise
;; chains for things that might otherwise loop... is this the right
;; thing to do?
(define* (await* fulfill-proc
                 #:key [promise? #f])
  (define-values (resume-flag val-or-err)
    (abort-to-prompt *actor-await-prompt* fulfill-proc promise?))
  (match resume-flag
    ['resume
     ;; it's a value, so let's return it
     val-or-err]
    ['error
     ;; it's an error, so let's raise it
     (throw 'unresumable-coroutine
            "Won't resume coroutine; got an *error* as a reply"
            #:error val-or-err)]))

(define* (await vow
                #:key
                [promise? #f])
  (define (fulfill-await resolver)
    (<-np resolver 'fulfill vow))
  (await* fulfill-await #:promise? promise?))

(define (<<- actor . args)
  (await (apply <- actor args)))


;; Spawning promises
;; =================

;; We've made the decision
(define already-resolved
  (lambda _ #f))

(define (^resolver bcom promise sealer)
  (match-lambda*
    [('fulfill val)
     (define sys (get-syscaller-or-die))
     (syscaller-fulfill-promise sys promise (sealer val))
     (bcom already-resolved)]
    [('break problem)
     (define sys (get-syscaller-or-die))
     (syscaller-break-promise sys promise (sealer problem))
     (bcom already-resolved)]))

(define* (_spawn-promise-and-resolver #:key
                                      (question-finder #f)
                                      (captp-connector #f))
  (define-values (sealer unsealer tm?)
    (make-sealer-triplet 'fulfill-promise))
  (define sys (get-syscaller-or-die))
  (define m-eventual
    (make-m~eventual unsealer tm?))
  (define m-unresolved
    (make-m~unresolved m-eventual '()))
  (define promise
    (let ((new-mactor
           (if question-finder
               (begin
                 (unless captp-connector
                   (error 'question-finder-without-captp-connector))
                 (make-mactor:question m-unresolved
                                       captp-connector
                                       question-finder))
               (make-mactor:naive m-unresolved '()))))
      (syscaller-spawn-mactor sys new-mactor #f)))
  (define resolver
    (spawn-named 'resolver ^resolver promise sealer))
  (values promise resolver))

;; We don't want to expose the keyword arguments of the parent
;; procedure to just everyone, hence this indirection
(define (spawn-promise-and-resolver)
  "Return a promise and its associated resolver.

Type: -> (Values Promise Resolver)"
  (_spawn-promise-and-resolver))

;; Deprecated
(define (spawn-promise-values)
  (issue-deprecation-warning
   "Spawn-promise-values is deprecated in favor of spawn-promise-and-resolver")
  (spawn-promise-and-resolver))
(define (spawn-promise-cons)
  "This procedure is deprecated, use spawn-promise-and-resolver instead."
  (issue-deprecation-warning
   "Spawn-promise-cons is deprecated in favor of spawn-promise-and-resolver")
  (call-with-values spawn-promise-and-resolver cons))


;; Spawning
;; ========

;; This is the internally used version of actormap-spawn,
;; also used by the syscaller.  It doesn't set up a syscaller
;; if there isn't currently one.
(define* (actormap-spawn!* actormap maybe-constructor
                           args
                           #:optional
                           [debug-name (procedure-name maybe-constructor)])
  (define vat-connector
    (actormap-vat-connector actormap))
  (define-values (become become-unseal become?)
    (make-become-sealer-triplet))
  (define-values (constructor constructor-refr)
    (values (if (redefinable-object? maybe-constructor)
                (redefinable-object-constructor maybe-constructor)
                maybe-constructor)
            maybe-constructor))
  (define actor-handler
    (apply constructor become args))
  (define* (handler->refr handler #:optional maybe-self-portrait)
    (match handler
      ;; We can't use match record unpacking because of goblin's $ function.
      [(? portraitized-behavior?)
       (handler->refr (portraitized-behavior-behavior handler)
                      (portraitized-behavior-self-portrait handler))]
      [(? procedure?)
       (let ((actor-refr
              (make-local-object-refr debug-name vat-connector
                                      (increment-actormap-aurie-counter! actormap))))
         (actormap-set! actormap actor-refr
                        (make-mactor:object handler constructor-refr
                                            constructor
                                            maybe-self-portrait
                                            become-unseal become?))
         actor-refr)]
      [(? live-refr? pre-existing-refr)
       pre-existing-refr]
      [_
       (error 'invalid-actor-handler "Not a procedure, aurie or live refr:" handler)]))
  (handler->refr actor-handler))

;; These two are user-facing procedures.  Thus, they set up
;; their own syscaller.

;; non-committal version of actormap-spawn
(define (actormap-spawn actormap actor-constructor . args)
  "Create and return a reference to ACTOR-CONSTRUCTOR inside ACTORMAP,
passing in ARGS; do not commit the transaction.

Type: Actormap Constructor Any ... -> Actor"
  (define new-actormap
    (make-transactormap actormap))
  (call-with-fresh-syscaller
   new-actormap
   (lambda (sys)
     (define actor-refr
       (actormap-spawn!* new-actormap actor-constructor
                         args))
     (values actor-refr new-actormap))))

(define (actormap-spawn! actormap actor-constructor . args)
  "Create and return a reference to ACTOR-CONSTRUCTOR inside ACTORMAP,
passing in ARGS; commit the transaction.

Type: Actormap Constructor Any ... -> Actor"
  (define new-actormap
    (make-transactormap actormap))
  (define actor-refr
    (call-with-fresh-syscaller
     new-actormap
     (lambda (sys)
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
        (make-local-object-refr debug-name vat-connector
                                (increment-actormap-aurie-counter! actormap))
        (make-local-promise-refr vat-connector)))
  (actormap-set! actormap actor-refr mactor)
  actor-refr)



;;; actormap turning and utils
;;; ==========================

(define (actormap-turn* actormap to-refr args)
  "Invoke TO-REFR with ARGS in ACTORMAP, without creating a new
generation. Return the result of the invoked behavior, a new Actormap,
and a list of new Messages.

Type: Actormap Actor Any ... ->
(Values Any Actormap (List Message ...))"
  (call-with-fresh-syscaller
   actormap
   (lambda (sys)
     (define result-val
       (syscaller-$ sys to-refr args))
     (values result-val
             (syscaller-actormap sys)
             (syscaller-new-msgs sys)))))

(define (actormap-turn actormap to-refr . args)
  "Invoke TO-REFR with ARGS in a new Actormap whose parent is
ACTORMAP. Return the result of the invoked behavior, a reference to
the new Actormap, and a list of new Messages.

Type: Actormap Actor Any ... ->
(Values Any Actormap (List Message ...))"
  (define new-actormap
    (make-transactormap actormap))
  (actormap-turn* new-actormap to-refr args))

;; run a turn but only for getting the result.
;; we're not interested in committing the result
;; so we discard everything but the result.
(define (actormap-peek actormap to-refr . args)
  "Invoke TO-REFR with ARGS in ACTORMAP only to return the results;
do not commit the transaction to the transaction history.

Type: Actormap Actor Any ... -> Any"
  (define-values (returned-val _am _nm)
    (actormap-turn* (make-transactormap actormap)
                    to-refr args))
  returned-val)

;; Note that this does nothing with the messages.
(define (actormap-poke! actormap to-refr . args)
  "Invoke TO-REFR with ARGS in ACTORMAP and commit the results, but do
not propagate any messages. Return the results.

Type: Actormap Actor Any ... -> Any"
  (define-values (returned-val transactormap _nm)
    (actormap-turn* (make-transactormap actormap)
                    to-refr args))
  (transactormap-merge! transactormap)
  returned-val)

(define (actormap-reckless-poke! actormap to-refr . args)
  "Invoke TO-REFR with ARGS in ACTORMAP, committing the results
directly to ACTORMAP reather than creating a new generation. Return
the results.

Type: Actormap Actor Any ... -> Any"
  (define-values (returned-val transactormap _nm)
    (actormap-turn* actormap to-refr args))
  returned-val)

;; like actormap-run but also returns the new actormap, new-msgs
(define (actormap-run* actormap thunk)
  "Evaluate THUNK in ACTORMAP and commit the results. Return the
results, the Actormap representing the latest generation of
transaction, and any messages generated.

Type: Actormap (-> Any) -> (Values Any Actormap (List Message ...))"
  (define-values (actor-refr new-actormap)
    (actormap-spawn (make-transactormap actormap) (lambda (bcom) thunk)))
  (define-values (returned-val new-actormap2 new-msgs)
    (actormap-turn* (make-transactormap new-actormap) actor-refr '()))
  (values returned-val new-actormap2 new-msgs))

;; non-committal version of actormap-run
(define (actormap-run actormap thunk)
  "Evaluate THUNK in ACTORMAP and return the results. Do not commit
the results to the transaction history.

Type: Actormap (-> Any) -> Any"
  (define-values (returned-val _am _nm)
    (actormap-run* (make-transactormap actormap) thunk))
  returned-val)

;; committal version
;; Run, and also commit the results of, the code in the thunk
(define* (actormap-run! actormap thunk
                        #:key [reckless? #f])
  "Evaluate THUNK in ACTORMAP and return the results. Commit the
results.

If RECKLESS? is #t, operate directly in ACTORMAP without creating a
new generation.

Type: Actormap (-> Any) (Optioan (#:reckless? Boolean)) -> Any"
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


(define while-handling-header
  "While attempting to handle message")
(define before-even-able-to-handle-header
  "Before even being able to handle message")
(define while-handling-listen-header
  "While handling listen request")

(define (simple-display-error msg err stack)
  (newline (current-error-port))
  (display ";; === Caught error: ===\n" (current-error-port))
  (format (current-error-port) ";;  message: ~s\n" msg)
  (format (current-error-port) ";;  exception: ~s\n" err)
  (display-backtrace* err stack))

(define (make-no-op msg)
  (lambda _ *unspecified*))

(define-exception-type &actormap-turn-error &error
  make-actormap-turn-error
  actormap-turn-error?
  (stack actormap-turn-error-stack))

(define* (actormap-turn-message actormap msg
                                #:key
                                [error-handler simple-display-error]
                                [reckless? #f]
                                [catch-errors? #t])
  "Invoke MSG in ACTORMAP and return the result.

If provided, ERROR-HANDLER is a procedure to handle exceptions.
If RECKLESS? is #t, operate directly in ACTORMAP without creating a
new generation; otherwise create a new generation of Actormap. If
CATCH-ERRORS? is #t, capture the stack and abort to a prompt;
otherwise propogate the error.

Type: Actormap Message (Optional (#:error-handler (Exception -> Any)))
(Optional (#:reckless? Boolean)) (Optional (#:catch-errors? Boolean))
-> Any"
  (define new-actormap
    (if reckless?
        actormap
        (make-transactormap actormap)))
  ;; TODO: Kuldgily reimplements part of actormap-turn*... maybe
  ;; there's some opportunity to combine things, dunno.
  (call-with-fresh-syscaller
   new-actormap
   (lambda (sys)
     (define (error-prompt-handler kont err stack-at-exn)
       ;; Since we threw an exception, we should inform that this
       ;; failed... if anyone cares
       (define resolve-me
         (message-who-wants-response msg))
       (define new-msgs
         (if resolve-me
             (list (make-message (syscaller-vat-connector sys)
                                 resolve-me #f (list 'break err)))
             '()))
       ;; Decorate the original exception with an actormap turn error
       ;; that captures the stack in which the original exception
       ;; occurred.
       (define turn-error
         (make-exception (make-actormap-turn-error stack-at-exn) err))
       (when error-handler
         (error-handler msg err stack-at-exn))
       (values `#(fail ,turn-error) actormap new-msgs))
     (define handle-exn-tag (make-prompt-tag 'goblins-turn))
     (define (catch-stack-and-abort-to-prompt err)
       ;; Prepare a slice of the call stack that only has the frames
       ;; relevant to the actor behavior that threw the error.  This
       ;; way we aren't exposing Goblins core stack frames, which
       ;; would be a security leak in a fully OCap secure system.
       (define stack
         (capture-current-stack
          #t ; get the current stack
          ;; Trim inner frames up to and including this
          ;; error handling procedure.
          catch-stack-and-abort-to-prompt
          ;; Trim outer frames up to the prompt tag.  This
          ;; hides *most* of the core frames.
          handle-exn-tag
          ;; The frame trimming arguments go inner, outer,
          ;; inner, outer, etc. so we need to no-op here so
          ;; we can trim more outer frames.
          0
          ;; Trim 3 more outer frames that the tag doesn't
          ;; eliminate for us.
          ;;
          ;; The frames are:
          ;;
          ;; - with-exception-handler
          ;; - do-call
          ;; - _handle-message or _handle-listen
          3))
       (abort-to-prompt handle-exn-tag err stack))
     (define (do-call)
       (define result
         (match msg
           [(? message?)
            (syscaller-handle-message sys msg)]
           [(? listen-request? lr)
            (syscaller-handle-listen sys
                                     (listen-request-to lr)
                                     (listen-request-listener lr)
                                     (listen-request-wants-partial? lr))]))
       (values `#(ok ,result) new-actormap (syscaller-new-msgs sys)))
     (if catch-errors?
         ;; We're catching errors?  Well, let's capture the stack without
         ;; unwinding, *then* abort to a prompt where it's safe to process
         ;; it...
         (call-with-prompt handle-exn-tag
           (lambda ()
             (with-exception-handler catch-stack-and-abort-to-prompt
               do-call
               #:unwind? #f
               #:unwind-for-type #t))
           error-prompt-handler)
         ;; No?  Well then, it's much simpler...
         (do-call)))))

(define* (actormap-churn am msg
                         #:key [catch-errors? #t]
                         ;; TODO: for consistency, replace with a #:reckless? flag
                         [make-transactormap? #t])
  "Perform every turn possible in AM to resolve MSG without needing to
send messages to far objects, then dispatch messages to far objects.

If CATCH-ERRORS is #t, collect the stack and abort to a prompt on
errors; otherwise, propogate errors.

If MAKE-TRANSACTORMAP? is #t, create a new generation for the
operation; otherwise, act directly in AM.

Type: Actormap Message (Optional (#:catch-errors? Boolean))
(Optional (#:make-transactormap? Boolean)) -> Void"
  (define churn-q (make-q))     ; message to churn on here
  ;; This one doesn't really need to be a queue.  Maybe it
  ;; makes things easier to think about though, I'm undecided.
  ;; TODO: Is our ordering really right for the final set of
  ;; things to send?
  (define send-far-q (make-q))  ; messages we must still send
  (define new-am
    (if make-transactormap?
        (make-transactormap am)
        am))
  (define this-vat-connector (actormap-vat-connector am))
  (define first-one? #t)
  (define first-return-val #f)
  (define (near-msg? msg)
    (define to-refr (message-or-request-to msg))
    (and (local-refr? to-refr)
         (eq? (local-refr-vat-connector to-refr)
              this-vat-connector)))
  ;; Used for both filling the initial queue and after
  ;; each turn... also used to queue up the messages to be
  ;; sent externally
  (define (q-append! q lst)
    (match lst
      ('() 'done)
      ((item rest ...)
       (q-push! q item)
       (q-append! q rest))))
  ;; Queue messages depending on whether they're for this actormap
  ;; or if they go somewhere else
  ;; TODO: We could probably be faster about this with an append or...
  ;; something.
  (define (queue-messages-appropriately! msgs)
    (match msgs
      ('() 'done)
      ((msg next-msgs ...)
       (queue-messages-appropriately! next-msgs)  ; last message first
       (if (near-msg? msg)
           (enq! churn-q msg)
           (enq! send-far-q msg)))))
  (define (churn!)
    (define next-msg (deq! churn-q))
    (define-values (this-result buffer-am new-msgs)
      (actormap-turn-message new-am next-msg
                             #:catch-errors? catch-errors?))
    (when first-one?
      (set! first-return-val this-result)
      (set! first-one? #f))
    ;; queue messages...
    (queue-messages-appropriately! new-msgs)
    ;; merge if appropriate...
    (match this-result
      [#('ok _result)
       (transactormap-buffer-merge! buffer-am)]
      [#('fail err) 'no-op])
    ;; and loop!
    (if (q-empty? churn-q)
        'done
        (churn!)))
  ;; Put the first message on the queue
  (enq! churn-q msg)
  ;; Turn as many times as it takes to run this
  ;; actormap turn / vat to quiescence
  (churn!)
  ;; And now let's return everything...
  (let ((send-far-msgs (car send-far-q)))
    (values first-return-val new-am send-far-msgs)))

(define* (actormap-churn-run actormap thunk
                             #:key [catch-errors? #t])
  "Evaluate THUNK in ACTORMAP, performing all possible invocations to
resolve THUNK without sending messages to far objects. Return the
results, a reference to an Actormap representing the new generation,
and any messages generated.

If CATCH-ERRORS? is #t, capture the stack and abort to a prompt on
error; otherwise, propogate the error.

Type: Actormap (-> Any) (Optional (#:catch-errors? Boolean)) ->
(Values Any Actormap (List Message))"
  (define vat-connector (actormap-vat-connector actormap))
  (define-values (actor-refr new-actormap)
    (actormap-spawn actormap (lambda (_bcom) thunk)))
  (define-values (returned-val _nam new-msgs)
    (actormap-churn new-actormap (make-message vat-connector actor-refr #f '())
                    #:catch-errors? catch-errors?
                    #:make-transactormap? #f))  ; reuses new-actormap
  (values returned-val new-actormap new-msgs))

(define-record-type <multival-return-kluge>
  (make-multival-return-kluge vals)
  multival-return-kluge?
  (vals multival-return-kluge-vals))

;; Also sends out relevant messages, and re-raises exceptions if appropriate
(define* (actormap-churn-run! actormap thunk
                              #:key [catch-errors? #t])
  "Evaluate THUNK in ACTORMAP, performing all possible invocations to
resolve THUNK without sending messages to far objects, then send out
messages. Return the results.

If CATCH-ERRORS? is #t, capture the stack and abort to a prompt on
error; otherwise, propogate the error.

Type: Actormap (-> Any) (Optional (#:catch-errors? Boolean)) -> Any"
  (define (churn-run-values->list . args)
    (call-with-values thunk
      (lambda rvals
        (make-multival-return-kluge rvals))))
  (define-values (returned-val new-actormap new-msgs)
    (actormap-churn-run actormap churn-run-values->list
                        #:catch-errors? catch-errors?))
  (dispatch-messages new-msgs)
  (match returned-val
    ;; kluge to handle the coroutine case
    [#('ok (? multival-return-kluge? mrk))
     (transactormap-merge! new-actormap)
     (apply values (multival-return-kluge-vals mrk))]
    [#('ok rval)
     (transactormap-merge! new-actormap)
     rval]
    [#('fail err)
     ;; re-raise exception
     (raise-exception err)]))

(define* (dispatch-message msg #:optional (timestamp 0))
  (cond
   ;; See the comment above <forward-to-captp> for why we're kind of
   ;; duplicating code with the final nested branch of this procedure.
   [(forward-to-captp? msg)
    ;; oh this is one of those klugey "forward me" things
    (let ((real-msg (forward-to-captp-msg msg))
          (captp-connector (forward-to-captp-connector msg)))
      (captp-connector 'handle-message real-msg))]
   [else
    ;; okay guess not
    (let ((to-refr (message-or-request-to msg)))
      (cond
       ;; send locally
       [(local-refr? to-refr)
        (match (local-refr-vat-connector to-refr)
          ;; TODO: When messages aren't going to be possible to deliver,
          ;; we should alert the waiting-on-message
          [(? procedure? vat-connector)
           (vat-connector 'handle-message timestamp msg)]
          ;; noplace like nowhere
          ;; TODO: Maybe we should give warnings about this, since
          ;; delivering messages to actors that can't receive them is...
          ;; surprising.
          [#f 'no-op])]
       ;; send remotely
       [else
        (let ((captp-connector (remote-refr-captp-connector to-refr)))
          (captp-connector 'handle-message msg))]))]))

(define (dispatch-messages msgs)
  (for-each dispatch-message msgs))

(define (syscaller-free proc)
  (parameterize ([current-syscaller #f])
    (proc)))

(define (persistence-env-compose . envs)
  "Composes a new persistence environment of the provided ENVS"
  (define constructor->object-spec
    (make-hash-table))
  (define name->object-spec
    (make-hash-table))

  (for-each
   (lambda (env)
     (hash-for-each
      (lambda (constructor object-spec)
        (let ((name (object-spec-name object-spec)))
          (hash-set! name->object-spec name object-spec)
          (hashq-set! constructor->object-spec constructor object-spec)))
      (persistence-env-constructor->object-spec env)))
   envs)

  (_make-persistence-env constructor->object-spec name->object-spec))

(define* (make-persistence-env* objects)
  "Constructs a new persistence environment from OBJECTS"
  (define constructor->object-spec
    (make-hash-table))
  (define name->object-spec
    (make-hash-table))

  (define* (add-object-to-env! name constructor #:optional rehydrator)
    (define object-spec
      (make-object-spec name constructor
                        (or rehydrator
                            (lambda (version . args)
                              (apply spawn constructor args)))))
    (hash-set! name->object-spec name object-spec)
    (hashq-set! constructor->object-spec constructor object-spec))

  (for-each
   (match-lambda
     [(name constructor)
      (add-object-to-env! name constructor)]
     [(name constructor rehydrator)
      (add-object-to-env! name constructor rehydrator)])
   objects)

  (_make-persistence-env constructor->object-spec name->object-spec))


(define* (make-persistence-env #:optional [objects '()] #:key extends)
  "Construct a new persistence environment containing all OBJECTS and all objects within EXTENDS"
  (let ((this-env (make-persistence-env* objects)))
    (match extends
      [#f this-env]
      [(? persistence-env?) (persistence-env-compose this-env extends)]
      [(_ ...) (apply persistence-env-compose this-env extends)])))

(define-syntax-rule (namespace-env namespace object ...)
  (make-persistence-env
   `(((namespace object) ,object) ...)))

(define (make-actormap-read-portrait! persistence-env roots)
  "Creates a read-portrait function for a given graph to take single object portraits of the graph.

Type: PersistenceEnv LiveRefr ... -> Procedure Procedure"
  (when (null? roots)
    (error "At least one root object must be specified to take a portrait"))

  (define slot->val
    (make-hash-table))

  (define (maybe-create-obj-slot! obj)
    "Looks up or creates slot for object"
    ;; Returns 2 values: (object-slot created?)
    (let ((slot (local-object-refr-aurie-id obj)))
      (if (hash-ref slot->val slot #f)
          (values slot #f)
          (begin
            (hash-set! slot->val slot obj)
            (values slot #t)))))

  (define root-slots
    (map (lambda (obj)
           (define-values (slot _created?)
             (maybe-create-obj-slot! obj))
           slot)
         roots))

  (define (read-portrait! am this-obj)
    (define slot (local-object-refr-aurie-id this-obj))
    (unless (hash-ref slot->val slot #f)
      (error "Object does not appear in the persistence graph" this-obj))

    ;; Keep track of new (previously not in the object graph) objects,
    ;; while this is represented as a hashmap, really it's working
    ;; like a set of new child objects.
    (define new-child-objs
      (make-hash-table))
    
    (define this-obj-self-portrait-fn
      (mactor:object-self-portrait (or (actormap-ref am this-obj)
                                       (error "Object not in actormap:" this-obj))))
    (define this-obj-constructor-refr
      (mactor:object-constructor-refr (actormap-ref am this-obj)))
    (define this-obj-spec
      (persistence-env-ref-by-constructor persistence-env this-obj-constructor-refr))
    (define obj-debug-name
      (local-object-refr-debug-name this-obj))

    (define (am-near-refr? refr)
      (actormap-run am (lambda () (near-refr? refr))))

    (define (am-far-refr? refr)
      (actormap-run am (lambda () (far-refr? refr))))
    
    (define (process-one value)
      (match value
        [(? depictable-atom? atom) atom]
        [(or (? pair?) ())
         ;; If the list is a regular list, don't tag it, but if it's a dotted list
         ;; we need to tag it as 'dotted-list.
         (let lp ((processed-list '())
                  (remaining value))
           (match remaining
             [()
              (reverse processed-list)]
             [(head . rest)
              (lp (cons (process-one head) processed-list) rest)]
             [last
              (make-tagged
               'dotl
               (lp (cons (process-one last) processed-list) '()))]))]
        [(? char?)
         (make-tagged* 'char (char->integer value))]
        [(? vector? vec)
         (make-tagged
          'vec
          (let lp ((i 0))
            (if (= i (vector-length vec))
                '()
                (cons (process-one (vector-ref vec i))
                      (lp (1+ i))))))]
        [(? ghash?)
         (ghash-fold
          (lambda (k v prev)
            (ghash-set prev (process-one k) (process-one v)))
          (make-ghash)
          value)]
        [(? gset?)
         (gset-fold
          (lambda (item prev)
            (gset-add prev (process-one item)))
          (make-gset)
          value)]
        [(? keyword? kw)
         (make-tagged* 'kw (keyword->symbol kw))]
        [(? tagged? tagged)
         (make-tagged* 'tagged
                       (tagged-label tagged)
                       (tagged-data tagged))]
        [(? zilch?)
         (make-tagged* 'zilch #f)]
        [(? unspecified?)
         (make-tagged* 'void #f)]
        [(and (? am-near-refr?) (? local-object-refr?))
         (let-values (((slot created?) (maybe-create-obj-slot! value)))
           (when created?
             (hashq-set! new-child-objs value #t))
           (make-tagged* 'near slot))]
        [(and (? am-far-refr? refr) (? local-object-refr?))
         ;; Far refrs need to be serailized as a tuple of:
         ;; (<vat-aurie-id> <refr-aurie-id>)
         (let* ((vat-connector (local-object-refr-vat-connector refr))
                (vat-aurie-id (and vat-connector (vat-connector 'aurie-vat-id)))
                (refr-aurie-id (local-object-refr-aurie-id refr)))
           (if vat-aurie-id
               (make-tagged* 'far vat-aurie-id refr-aurie-id)
               (make-tagged* 'broken)))]
        [(? local-promise-refr? vow)
         (actormap-run
          am
          (lambda ()
            (if (near-promise-settled? vow #:broken-ok? #f)
                (let* ((inner (near-settled-promise-value vow))
                       (processed-inner (process-one inner)))
                  (if (and (tagged? processed-inner)
                           (or (eq? (tagged-label processed-inner) 'near)
                               (eq? (tagged-label processed-inner) 'far)))
                      processed-inner
                      (make-tagged* 'encase processed-inner)))
                (make-tagged* 'broken))))]
        [(? ocapn-id?)
         (make-tagged* 'ocapn-id (ocapn-id->string value))]
        [(? persistable-object-identifier?)
         (make-tagged* 'persistable-obj-id
                       (persistable-object-identifier-vat-id value)
                       (persistable-object-identifier-object-id value))]
        [_ (error "Unserializable value!" 'value: value 'obj this-obj)]))
    
    (define (process-portrait obj-spec portrait-data)
      (unless obj-spec
        (error "Don't know how to persist:" this-obj this-obj-constructor-refr))
      (match portrait-data
        [(? versioned-data? data)
         (define-values (portrait-version portrait-data)
           (values (versioned-data-version data) (versioned-data-data data)))
         (list (object-spec-name obj-spec)
               obj-debug-name
               portrait-version
               (process-one portrait-data))]
        [(? list? args)
         ;; No versioning was given, lets tag this as version 0
         (process-portrait obj-spec (versioned 0 args))]))

    (define returned-self-portrait
      (if this-obj-self-portrait-fn
          (actormap-run am this-obj-self-portrait-fn)
          (error "No self portrait function found for object" this-obj)))

    (define depiction-to-save
      (process-portrait this-obj-spec returned-self-portrait))

    (values slot depiction-to-save new-child-objs))

  ;; Really, this changed; the main purpose of this is to avoid giving
  ;; aurie ids away unless you're actually working with Aurie itself.
  (define val->slot-ref
    (case-lambda
      ((obj default-value)
       (let ((slot (local-object-refr-aurie-id obj)))
         ;; return the slot if this is indeed an object in the aurie
         ;; table, otherwise return default value
         (or (and (hash-ref slot->val slot #f)
                  slot)
             default-value)))
      ((obj)
       (let ((slot (local-object-refr-aurie-id obj)))
         (and (hash-ref slot->val slot #f)
              slot)))))

  (values read-portrait! val->slot-ref))

(define (actormap-take-portrait-with-read-portrait am read-portrait!
                                                   obj->slot-ref roots)
  "Take a portrait of the graph with a given read-portrait! function

Type: Actormap Procedure Procedure LiveRef ... -> Hashmap List"
  (define slot->portrait
    (make-hash-table))
  (define process-queue
    (make-q))
  ;; Looking up if an item is in the process queue is an O(n^2)
  ;; operation. Since performance matters here, make a cache.
  (define process-queue-cache
    (make-hash-table))

  (define (maybe-enqueue obj)
    (unless (hashq-ref process-queue-cache obj #f)
      (enq! process-queue obj)))

  ;; Queue up all the roots
  (for-each maybe-enqueue roots)

  ;; Go through the queue of objects to read and process them.
  (while (not (q-empty? process-queue))
    (let*-values ([(obj) (deq! process-queue)]
                  [(slot obj-self-portrait new-child-objs) (read-portrait! am obj)])
      (hashq-set! slot->portrait slot obj-self-portrait)

      ;; We need to queue up new child objects which are not currently queued for
      ;; processing.
      (hash-for-each
       (lambda (obj _value)
         (maybe-enqueue obj))
       new-child-objs)))

  (define root-slots
    (map obj->slot-ref roots))

  (values slot->portrait root-slots))

(define (actormap-take-portrait am persistence-env . roots)
  "Produces a self portrait of the actormap"
  (define-values (read-portrait! obj->slot-ref)
    (make-actormap-read-portrait! persistence-env roots))

  (actormap-take-portrait-with-read-portrait
   am read-portrait! obj->slot-ref roots))

(define (actormap-replace-behavior am persistence-env)
  "Functional version of `actormap-replace-behavior!'

This works the same as `actormap-replace-behavior!' but it returns
a transactormap which the user can choose whether or not to commit.

Type: Actormap PersistenceEnv -> TransactorMap"
  (define metatype (actormap-metatype am))
  (define actormap-for-each
    (actormap-metatype-for-each-proc metatype))
  (define new-actormap (make-transactormap am))

  (define (has-new-beh? object-spec mactor)
    (define spawned-constructor
      (mactor:object-spawned-constructor mactor))
    (define current-constructor
      (object-spec-constructor object-spec))
    ;; We've already checked before this procedure is invoked whether or not
    ;; this is a `redefinable-object?' so we don't need to do that again.
    (not (eq? (redefinable-object-constructor current-constructor)
              spawned-constructor)))

  (define (dispatch-messages-for-am! am messages)
    (define msg-queue (make-q))
    (define (queue-messages! msgs)
      (for-each
       (lambda (msg)
         (enq! msg-queue msg))
       msgs))
    (queue-messages! messages)
    (while (not (q-empty? msg-queue))
      (let-values (((_val new-am new-msgs)
                    (actormap-turn-message am (deq! msg-queue))))
        (queue-messages! new-msgs)
        (transactormap-merge! new-am))))

  (actormap-for-each
   (lambda (refr mactor)
     ;; Find the object spec for the given mactor (might not have one).
     (define object-spec
       (if (and (mactor:object? mactor)
                (redefinable-object? (mactor:object-constructor-refr mactor)))
           (persistence-env-ref-by-constructor
            persistence-env
            (mactor:object-constructor-refr mactor))
           #f))

     (when (and object-spec (has-new-beh? object-spec mactor))
       (let* ([take-self-portrait (mactor:object-self-portrait mactor)]
              [self-portrait (take-self-portrait)]
              [rehydrator (object-spec-rehydrator object-spec)])
         ;; The rehydrator will call `spawn' which will create a new refr,
         ;; but we actually want to keep the old refr.  Once we've rehydrated
         ;; the actor, install the new object at its old refr.
         (define versioned-self-portrait
           (if (versioned-data? self-portrait)
               self-portrait
               (versioned 0 self-portrait)))
         ;; Restoring but we need to commit this, for two important reasons:
         ;; 1. The actor may spawn other actors which need to stick around
         ;; 2. If the actor gives itself to another actor, that refr
         ;;    needs to continue to work.
         (define-values (tmp-refr new-am* new-msgs)
           (actormap-run*
            new-actormap
            (lambda ()
              (apply rehydrator
                     (versioned-data-version versioned-self-portrait)
                     (versioned-data-data versioned-self-portrait)))))

         ;; Set the old refr to point to this new mactor we
         ;; spawned. We could technically set this to a local-link but
         ;; in most cases `temp-refr' will be Gc'd and this will be
         ;; the sole refr remaining.
         (actormap-set! new-am* refr
                        (actormap-ref new-am* tmp-refr))
         ;; We do still care about making temp-refr still work if it
         ;; was given out, see reason number 2 above so set it to a
         ;; local-link.
         (actormap-set! new-am* tmp-refr
                        (make-mactor:local-link refr))

         (dispatch-messages-for-am! new-am* (reverse new-msgs))
         (transactormap-merge! new-am*))))
   am)
  new-actormap)

(define (actormap-replace-behavior! am persistence-env)
  "Replace actors in actormap AM with new behavior from PERSISTENCE-ENV

Takes self portrait of all the actors with different behavior and
rehydrates them with the new behavior.

Type: Actormap PersistenceEnv -> Void"
  (define tm (actormap-replace-behavior am persistence-env))
  (transactormap-merge! tm))

(define (depictable-atom? obj)
  (or (number? obj) (boolean? obj) (string? obj)
      (symbol? obj) (bytevector? obj)))

(define (actormap-restore! am persistence-env portraits roots)
  (define-values (_far-refrs root-objects)
    (actormap-restore-with-far-refrs! am persistence-env portraits roots))
  (apply values root-objects))

(define (actormap-restore-with-far-refrs! am persistence-env portraits roots)
  "Restore a self portrait in an actormap"
  (define slots->resolvers
    (make-hash-table))
  (define slots->refrs
    (make-hash-table))

  ;; This is to restore the actormaps aurie-id counter which is used
  ;; to give new objects a unique ID within the actormap. It should be
  ;; above all the persisted object IDs.
  (define highest-slot 0)

  ;; Handle restoring far refrs
  (define far-refr-resolvers
    (make-hash-table))
  (define far-refr-vows
    (make-hash-table))

  ;; TODO: Make a more generalized approach to "churn" code.
  ;; There are lots of places around the code base which does
  ;; something similar to this "churn" mechanism. There's actormap
  ;; churn code, vat churn stuff, update behavior and this.
  (define near-msg-queue (make-q))
  (define far-msg-queue (make-q))
  (define (enq-msgs! msgs)
    (define (am-near-refr? refr)
      (actormap-run am (lambda () (near-refr? refr))))
    (define (near-msg? msg)
      (define to-refr (message-or-request-to msg))
      (am-near-refr? to-refr))
    (for-each
     (lambda (msg)
       (if (near-msg? msg)
           (enq! near-msg-queue msg)
           (enq! far-msg-queue msg)))
     msgs))

  (define (depiction->debug-name depiction)
    ;; All depictions should be objects
    (match depiction
      [(_persistence-name debug-name _portrait-version _portrait-data)
       debug-name]))
  
  ;; We *need* to ensure we set the aurie-id counter on the actormap to the
  ;; highest within the graph before creating any new local-object-refrs.
  ;; Unfortunately that means having a pass over the graph just to calculate
  ;; the highest aurie ID for the counter.
  ;; TODO: Do we possibly want to serialize the counter so we don't need to do
  ;; this? - not sure.
  (hash-for-each
   (lambda (slot depiction)
     (when (< highest-slot slot)
       (set! highest-slot slot)))
    portraits)
  (set-actormap-aurie-counter! am highest-slot)

  ;; Now loop through and make a refr for each object, we will point that refr
  ;; at a vow and later change it to point directly at the refr.
  (hash-for-each
   (lambda (slot depiction)
     (let*-values (((vow resolver) (actormap-run! am spawn-promise-and-resolver))
                   ((vow-symlink) (make-mactor:local-link vow))
                   ((debug-name) (depiction->debug-name depiction))
                   ((vat-connector) (actormap-vat-connector am))
                   ((refr) (make-local-object-refr debug-name vat-connector slot)))
       (actormap-set! am refr vow-symlink)
       (hashq-set! slots->resolvers slot resolver)
       (hashq-set! slots->refrs slot refr)))
   portraits)

  (define (restore-slot! slot portrait)
    (define resolver
      (hashq-ref slots->resolvers slot))
    (define refr
      (hashq-ref slots->refrs slot))
    (define-values (obj-name obj-debug-name obj-portrait-version obj-portrait)
      (match portrait
        [(name debug-name portrait-version portrait-data)
         (values name debug-name portrait-version portrait-data)]
        [_ (error "Unknown portrait data")]))
    (define restored-args
      (actormap-run! am (lambda () (restore-one obj-portrait))))
    (define obj-spec
      (persistence-env-ref persistence-env obj-name))
    (define rehydrator
      (object-spec-rehydrator obj-spec))

    (define (restore-one depicted)
      (match depicted
        [(? tagged? depiction)
         (let ([type (tagged-label depiction)]
               [data (tagged-data depiction)])
           (match type
             ['dotl
              (let lp ((remaining data))
                (match remaining
                  [(last) (restore-one last)]
                  [(head . rest)
                   (cons (restore-one head) (lp rest))]))]
             ['vec
              (let ((vec (make-vector (length data))))
                (let lp ((index 0)
                         (lst data))
                  (match lst
                    (() vec)
                    ((head . tail)
                     (vector-set! vec index (restore-one head))
                     (lp (+ 1 index) tail)))))]
             ['char (integer->char (car data))]
             ['list (map restore-one data)]
             ['kw (symbol->keyword (car data))]
             ['zilch zilch]
             ['void *unspecified*]
             ['tagged
              (match data
                [(label payload)
                 (make-tagged label payload)])]
             ['near (hashq-ref slots->refrs (car data))]
             ['far
              ;; Cache the vow since we may have several refrences to the same obj
              (match (hash-ref far-refr-vows data #f)
                (#f
                 (let-values (((vow resolver) (spawn-promise-and-resolver)))
                   (hash-set! far-refr-resolvers data resolver)
                   (hash-set! far-refr-vows data vow)
                   vow))
                (vow vow))]
             ['encase
              ;; This is a promise which contains a value, re-encase
              ;; in a promise and return that.
              (let-values (((vow resolver) (spawn-promise-and-resolver)))
                ($ resolver 'fulfill (restore-one (car data)))
                vow)]
             ['broken
              (let-values (((vow resolver) (spawn-promise-and-resolver)))
                ;; TODO: An actual error type?
                ($ resolver 'break "Aurie broken promise")
                vow)]
             ['ocapn-id (string->ocapn-id (car data))]
             ['persistable-obj-id
              (match data
                [(vat-id object-id)
                 (make-persistable-object-identifier vat-id object-id)])]
             [_ (error "Unknown depiction type" type)]))]
        [(? ghash?)
         (ghash-fold
          (lambda (k v prev)
            (ghash-set prev (restore-one k) (restore-one v)))
          (make-ghash)
          depicted)]
        [(? gset?)
         (gset-fold
          (lambda (item prev)
            (gset-add prev (restore-one item)))
          (make-gset)
          depicted)]
        [(? list?) (map restore-one depicted)]
        [(? depictable-atom?) depicted]
        [_ (error "Unknown value in portrait data" depicted)]))

    (define-values (restored-obj-refr new-am new-msgs)
      (actormap-run*
       am
       (lambda ()
         (let ((restored-obj (apply rehydrator obj-portrait-version restored-args)))
           ($ resolver 'fulfill restored-obj)
           restored-obj))))

    (transactormap-merge! new-am)
    (enq-msgs! (reverse new-msgs))

    ;; Install the mactor in the refr we created.
    (actormap-set! am refr (actormap-ref am restored-obj-refr)))

  ;; Restore all the objects in the vows we have setup.
  (hash-for-each
   (lambda (slot portrait)
     (restore-slot! slot portrait))
   portraits)

  ;; When an actor is spawned it might send messages
  ;; so keep track of those so we can dispatch them after.
  (while (not (q-empty? near-msg-queue))
    (let-values (((result new-am new-msgs)
                  (actormap-turn-message am (deq! near-msg-queue))))
      (match result
        (#('ok _)
         (transactormap-merge! new-am))
        (_ (values)))
      (enq-msgs! new-msgs)))

  ;; Dispatch the far messages.
  (while (not (q-empty? far-msg-queue))
         (let ((msg (deq! far-msg-queue)))
           (dispatch-message msg)))

  (match roots
    [(? list? root-slots)
     (define restored-roots
       (map (lambda (slot)
              (hashq-ref slots->refrs slot))
            root-slots))
     (values far-refr-resolvers restored-roots)]
    [(? integer? slot)
     (values far-refr-resolvers (list (hashq-ref slots->refrs slot)))]))

(define (actormap-restore-from-store! am env store)
  "Reads portrait graph from STORE and restores into provided AM.

Returns the root objects of the graph."
  (define read-proc
    (persistence-store-read-proc store))
  (define-values (vat-aurie-id roots-version portraits slots)
    (read-proc 'graph-and-slots))
  (actormap-restore! am env portraits slots))

(define (actormap-save-to-store! am env store . roots)
  "Take portraits of ROOTS and save them into STORE"
  (define save-proc
    (persistence-store-save-proc store))
  (define-values (portraits slots)
    (apply actormap-take-portrait am env roots))
  (save-proc 'save-graph #f 0 portraits slots))

(define (local-refr->persistable-object-identifier local-refr)
  (define vat-connector
    (local-refr-vat-connector local-refr))
  (unless vat-connector
    (error "local-refr ~a has no vat connector" local-refr))
  (make-persistable-object-identifier
   (vat-connector 'aurie-vat-id)
   (local-object-refr-aurie-id local-refr)))

(define (has-persistable-object-identifier? local-refr)
  (define vat-connector
    (local-refr-vat-connector local-refr))
  (and vat-connector
       (vat-connector 'aurie-vat-id)))
