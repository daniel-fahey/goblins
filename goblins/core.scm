;;; Copyright 2019-2023 Christine Lemmer-Webber
;;; Copyright 2023 David Thompson
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

(define-module (goblins core)
  #:export (live-refr?
            local-refr?
            remote-refr?
            promise-refr?
            local-object-refr?
            local-promise-refr?
            remote-object-refr?
            remote-promise-refr?

            near-refr?
            far-refr?

            make-actormap
            make-transactormap
            make-whactormap

            actormap-vat-connector

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

            whactormap?
            transactormap?
            transactormap-reparent
            transactormap-merge!
            transactormap-buffer-merge!

            copy-whactormap

            spawn spawn-named
            $ <-np <-
            on

            <-np-extern
            listen-to

            await await*
            <<-

            spawn-promise-cons
            spawn-promise-values

            ;; TODO: separate this out!
            <message>
            make-message message?
            message-from-vat
            message-to
            message-resolve-me
            message-args

            <questioned>
            questioned?
            questioned-message questioned-answer-this-question

            <listen-request>
            make-listen-request listen-request?
            listen-request-from-vat
            listen-request-to
            listen-request-listener
            listen-request-wants-partial?

            message-or-request-from-vat
            message-or-request-to
            message-who-wants-response

            syscaller-free

            ;; TODO: These really should be moved into a more private
            ;; location...!  Few things will need, or should have, this
            make-remote-object-refr
            make-remote-promise-refr
            local-object-refr-debug-name
            local-refr-vat-connector
            remote-refr-captp-connector
            remote-refr-sealed-pos

            near-promise-broken?
            near-promise-settled?
            near-settled-promise-value
            near-promise-resolved?
            near-resolved-promise-value)
  #:replace (spawn)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 q)
  #:use-module (ice-9 suspendable-ports))


;;; Utilities (which should be moved to their own modules)
;;; ======================================================

;;; Here's basically your pre-goblins area.

;; Old hack to get the "unspecified/undefined type"
(define _void (if #f #f))

;; mimic Racket's seteq
(define (vseteq . items)
  (alist->vhash (map (lambda (x) (cons x #t)) items) hashq))
(define (vseteq-add vseteq item)
  (vhash-consq item #t vseteq))
(define (vseteq-member? vseteq item)
  (vhash-assq item vseteq))

;; (TODO: Use from (goblins simple-sealers) when we break
;; into modules.  For now we want to demonstrate stages as quasi-self-contained.)

(define* (make-sealer-triplet #:optional name)
  (define-record-type <seal>
    (seal val)
    sealed?
    (val unseal))
  (set-record-type-printer!
   <seal>
   (lambda (record port)
     (if name
         (begin
           (display "<sealed: " port)
           (display name port)
           (display ">" port))
         (display "<sealed>"))))
  (values seal unseal sealed?))



;;;                  .============================.
;;;                  | High level view of Goblins |
;;;                  '============================'
;;;
;;; There's a lot of architecture here.
;;; It's a lot to take in, so let's start out with a "high level view".
;;; Here's an image to get started:
;;;
;;;   .----------------------------------.         .-------------------.
;;;   |            Machine 1             |         |     Machine 2     |
;;;   |            =========             |         |     =========     |
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
;;;  - Zooming out the farthest is the "machine/network level".
;;;    There are two machines (Machine 1 and Machine 2) connected over a
;;;    Goblins CapTP network.  The stubby shapes on the borders between the
;;;    machines represent the directions of references Machine 1 has to
;;;    objects in Machine 2 (at the top) and references Machine 2 has to
;;;    Machine 1.  Both machines in this diagram are cooperating to preserve
;;;    that Bob has access to Carol but that Carol does not have access to
;;;    Bob, and that Carlos has access to Bob but Bob does not have access
;;;    to Carlos.  (However there is no strict guarantee from either
;;;    machine's perspective that this is the case... generally it's in
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
;;;
;;;
;;;                  .============================.
;;;                  | Goblins abstraction layers |
;;;                  '============================'
;;;
;;; Generally, things look like so:
;;;
;;;   (machine (vat (actormap {refr: (mactor object-handler)})))
;;;
;;; However, we could really benefit from looking at those in more detail,
;;; so from the outermost layer in...
;;;
;;;    .--- A machine in Goblins is basically an OS process.
;;;    |    However, the broader Goblins CapTP/MachineTP network is
;;;    |    made up of many machines.  A connection to another machine
;;;    |    is the closest amount of "assurance" a Goblins machine has
;;;    |    that it is delivering to a specific destination.
;;;    |    Nonetheless, Goblins users generally operate at the object
;;;    |    reference level of abstraction, even across machines.
;;;    |
;;;    |    An object reference on the same machine is considered
;;;    |    "local" and an object reference on another machine is
;;;    |    considered "remote".
;;;    |
;;;    |      .--- Christine: "How about I call this 'hive'?"
;;;    |      |    Ocap community: "We hate that, use 'vat'"
;;;    |      |    Everyone else: "What's a 'vat' what a weird name"
;;;    |      |
;;;    |      |    A vat is a traditional ocap term, both a container for
;;;    |      |    objects but most importantly an event loop that
;;;    |      |    communicates with other event loops.  Vats operate
;;;    |      |    "one turn at a time"... a toplevel message is handled
;;;    |      |    for some object which is transactional; either it happens
;;;    |      |    or, if something bad happens in-between, no effects occur
;;;    |      |    at all (except that a promise waiting for the result of
;;;    |      |    this turn is broken).
;;;    |      |
;;;    |      |    Objects in the same vat are "near", whereas objects in
;;;    |      |    remote vats are "far".  (As you may notice, "near" objects
;;;    |      |    can be "near" or "far", but "remote" objects are always
;;;    |      |    "far".)
;;;    |      |
;;;    |      |    This distinction is important, because Goblins supports
;;;    |      |    both asynchronous messages + promises via `<-` and
;;;    |      |    classic synchronous call-and-return invocations via `$`.
;;;    |      |    However, while any actor can call any other actor via
;;;    |      |    <-, only near actors may use $ for synchronous call-retun
;;;    |      |    invocations.  In the general case, a turn starts by
;;;    |      |    delivering to an actor in some vat a message passed with <-,
;;;    |      |    but during that turn many other near actors may be called
;;;    |      |    with $.  For example, this allows for implementing transactional
;;;    |      |    actions as transferring money from one account/purse to another
;;;    |      |    with $ in the same vat very easily, while knowing that if
;;;    |      |    something bad happens in this transaction, no actor state
;;;    |      |    changes will be committed (though listeners waiting for
;;;    |      |    the result of its transaction will be informed of its failure);
;;;    |      |    ie, the financial system will not end up in a corrupt state.
;;;    |      |    In this example, it is possible for users all over the network
;;;    |      |    to hold and use purses in this vat, even though this vat is
;;;    |      |    responsible for money transfer between those purses.
;;;    |      |    For an example of such a financial system in E, see
;;;    |      |    "An Ode to the Grannovetter Diagram":
;;;    |      |      http://erights.org/elib/capability/ode/index.html
;;;    |      |
;;;    |      |    .--- Earlier we said that vats are both an event loop and
;;;    |      |    |    a container for storing actor state.  Surprise!  The
;;;    |      |    |    vat is actually wrapping the container, which is called
;;;    |      |    |    an "actormap".  While vats do not expose their actormaps,
;;;    |      |    |    Goblins has made a novel change by allowing actormaps to
;;;    |      |    |    be used as independent first-class objects.  Most users
;;;    |      |    |    will rarely do this, but first-class usage of actormaps
;;;    |      |    |    is still useful if integrating Goblins with an existing
;;;    |      |    |    event loop (such as one for a video game or a GUI) or for
;;;    |      |    |    writing unit tests.
;;;    |      |    |
;;;    |      |    |    The keys to actormaps are references (called "refrs")
;;;    |      |    |    and the values are current behavior.  This is described
;;;    |      |    |    below.
;;;    |      |    |
;;;    |      |    |    Actormaps also technically operate on "turns", which are
;;;    |      |    |    a transactional operation.  Once a turn begins, a dynamic
;;;    |      |    |    "syscaller" (or "actor context") is initialized so that
;;;    |      |    |    actors can make changes within this transaction.  At the
;;;    |      |    |    end of the turn, the user of actormap-turn is presented
;;;    |      |    |    with the transactional actormap (called "transactormap")
;;;    |      |    |    which can either be committed or not to the current mutable
;;;    |      |    |    actormap state ("whactormap", which stands for
;;;    |      |    |    "weak hash actormap"), alongside a queue of messages that
;;;    |      |    |    were scheduled to be run from actors in this turn using <-,
;;;    |      |    |    and the result of the computation run.
;;;    |      |    |
;;;    |      |    |    However, few users will operate using actormap-turn directly,
;;;    |      |    |    and will instead either use actormap-poke! (which automatically
;;;    |      |    |    commits the transaction if it succeeds or propagates the error)
;;;    |      |    |    or actormap-peek (which returns the result but throws away the
;;;    |      |    |    transaction; useful for getting a sense of what's going on
;;;    |      |    |    without committing any changes to actor state).
;;;    |      |    |    Or, even more commonly, they'll just use a vat and never think
;;;    |      |    |    about actormaps at all.
;;;    |      |    |
;;;    |      |    |         .--- A reference to an object or actor.
;;;    |      |    |         |    Traditionally called a "ref" by the ocap community, but
;;;    |      |    |         |    scheme already uses "-ref" everywhere so we call it
;;;    |      |    |         |    "refr" instead.  Whatever.
;;;    |      |    |         |
;;;    |      |    |         |    Anyway, these are the real "capabilities" of Goblins'
;;;    |      |    |         |    "object capability system".  Holding onto one gives you
;;;    |      |    |         |    authority to make invocations with <- or $, and can be
;;;    |      |    |         |    passed around to procedure or actor invocations.
;;;    |      |    |         |    Effectively the "moral equivalent" of a procedure
;;;    |      |    |         |    reference.  If you have it, you can use (and share) it;
;;;    |      |    |         |    if not, you can't.
;;;    |      |    |         |
;;;    |      |    |         |    Actually, technically these are local-live-refrs...
;;;    |      |    |         |    see "The World of Refrs" below for the rest of them.
;;;    |      |    |         |
;;;    |      |    |         |      .--- We're now at the "object behavior" side of
;;;    |      |    |         |      |    things.  I wish I could avoid talking about
;;;    |      |    |         |      |    "mactors" but we're talking about the actual
;;;    |      |    |         |      |    implementation here so... "mactor" stands for
;;;    |      |    |         |      |    "meta-actor", and really there are a few
;;;    |      |    |         |      |    "core kinds of behavior" (mainly for promises
;;;    |      |    |         |      |    vs object behavior).  But in the general case,
;;;    |      |    |         |      |    most objects from a user's perspective are the
;;;    |      |    |         |      |    mactor:object kind, which is just a wrapper
;;;    |      |    |         |      |    around the current object handler (as well as
;;;    |      |    |         |      |    some information to track when this object is
;;;    |      |    |         |      |    "becoming" another kind of object.
;;;    |      |    |         |      |
;;;    |      |    |         |      |      .--- Finally, "object"... a term that is
;;;    |      |    |         |      |      |    unambiguous and well-understood!  Well,
;;;    |      |    |         |      |      |    "object" in our system means "references
;;;    |      |    |         |      |      |    mapping to an encapsulation of state".
;;;    |      |    |         |      |      |    Refrs are the reference part, so
;;;    |      |    |         |      |      |    object-handlers are the "current state"
;;;    |      |    |         |      |      |    part.  The time when an object transitions
;;;    |      |    |         |      |      |    from "one" behavior to another is when it
;;;    |      |    |         |      |      |    returns a new handler wrapped in a "become"
;;;    |      |    |         |      |      |    wrapper specific to this object (and
;;;    |      |    |         |      |      |    provided to the object at construction
;;;    |      |    |         |      |      |    time)
;;;    |      |    |         |      |      |
;;;    V      V    V         V      V      V
;;; (machine (vat (actormap {refr: (mactor object-handler)})))
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
;;; process runtime or between machines across captp sessions) and
;;; offline-storeable references (sturdy refrs, a kind of bearer URI,
;;; and certificate chains, which are like "deeds" indicating that the
;;; possessor of some cryptographic material is permitted access).
;;;
;;; All offline-storeable references must first be converted to live
;;; references before they can be used (authority to do this itself a
;;; capability, as well as authority to produce these offline-storeable
;;; objects).
;;;
;;; Live references subdivide into local (on the same machine) and
;;; remote (on a foreign machine).  These are typed as either
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

;; (set-record-type-printer!
;;  <actormap>
;;  (lambda (am port)
;;    (format port "#<actormap ~a>" (actormap-metatype-name (actormap-metatype am)))))

(define-record-type <actormap-metatype>
  (make-actormap-metatype name ref-proc set!-proc)
  actormap-metatype?
  (name actormap-metatype-name)
  (ref-proc actormap-metatype-ref-proc)
  (set!-proc actormap-metatype-set!-proc))

(define (actormap-set! am key val)
  ((actormap-metatype-set!-proc (actormap-metatype am))
   am key val)
  _void)

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
  "Create and return a reference to a weak-hash actormap. If provided,
VAT-CONNECTOR is the syscaller of the containing vat.

Type: (Optional Syscaller) -> WHActormap"
  (_make-actormap whactormap-metatype
                  (make-whactormap-data (make-weak-key-hash-table))
                  vat-connector))

(define (whactormap? obj)
  "Return #t if OBJ is a weak-hash actormap, else #f.

Type: Any -> Boolean"
  (and (actormap? obj)
       (eq? (actormap-metatype obj) whactormap-metatype)))

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
                  (actormap-vat-connector am)))


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
  _void)

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
      (set-transactormap-data-merged?! tm-data #t))
    root-actormap)
  (do-merge! transactormap)
  _void)

(define (transactormap-buffer-merge! transactormap)
  "Merge TRANSACTORMAP against its parent buffer (also a transactormap)"
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
  (set-transactormap-data-merged?! tm-data #t))

(define transactormap-metatype
  (make-actormap-metatype 'transactormap transactormap-ref transactormap-set!))

(define (make-transactormap parent)
  "Create a return a reference to a transactional actormap
representing the generation after PARENT.

Type: Actormap -> TransActormap"
  (define vat-connector (actormap-vat-connector parent))
  (_make-actormap transactormap-metatype
                  (make-transactormap-data parent (make-hash-table) #f)
                  vat-connector))

(define (transactormap-reparent transactormap new-parent)
  (define vat-connector (actormap-vat-connector new-parent))
  (define delta (transactormap-data-delta (actormap-data transactormap)))
  (_make-actormap transactormap-metatype
                  (make-transactormap-data new-parent delta #f)
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
;; to the local machine representative actor, but also has something
;; serialized that knows which specific remote machine + session this
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



;; "Become" sealer/unsealers
;; =========================

(define (make-become-sealer-triplet)
  (define-record-type <become-seal>
    (make-become-seal new-behavior return-val)
    become-sealed?
    (new-behavior unseal-behavior)
    (return-val unseal-return-val))
  (define* (become new-behavior #:optional [return-val _void])
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
  (make-mactor:object behavior become-unsealer become?)
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
         (sys 'near-refr? obj))))
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
  ((current-syscaller) 'near-mactor refr))



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

(define (fresh-syscaller actormap)
  (define vat-connector
    (actormap-vat-connector actormap))
  (define new-msgs '())

  (define (queue-new-msg! new-msg)
    (set! new-msgs (cons new-msg new-msgs)))

  (define closed? #f)

  (define (this-syscaller method-id . args)
    (define method
      (case method-id
        [($) _$]
        [(spawn) _spawn]
        [(<-) _<-]
        [(<-np) _<-np]
        [(spawn-mactor) spawn-mactor]
        [(send-message) _send-message]
        ;; TODO:
        [(fulfill-promise) fulfill-promise]
        [(break-promise) break-promise]
        [(handle-message) _handle-message]
        [(handle-listen) _handle-listen]
        [(send-listen) _send-listen]
        [(on) _on]
        [(vat-connector) get-vat-connector]
        [(near-refr?) near-refr?]
        [(near-mactor) near-mactor]
        [else (error 'invalid-syscaller-method
                     method-id)]))
    (when closed?
      (error "Syscaller closed business while processing:"
             method-id args))
    (apply method args))

  ;; TODO
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
      (error 'no-such-actor "no actor with this id in this vat:" to-refr))
    mactor)

  ;; call actor's behavior
  (define (_$ to-refr args)
    ;; Restrict to live-refrs which appear to have the same
    ;; vat-connector as us
    (unless (local-refr? to-refr)
      (error 'not-callable
             "Not a live reference:" to-refr))

    (unless (eq? (local-refr-vat-connector to-refr)
                 vat-connector)
      (error 'not-callable
             "Not in the same vat:" to-refr))

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
         (define (_do-actor-call)
           (apply actor-behavior args))
         (define (_handle-await k fulfill-proc promise?)
           (define-values (waiting-promise waiting-resolver)
             (_spawn-promise-values))
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
         (define-values (new-behavior return-val)
           (let ([returned
                  (call-with-prompt *actor-await-prompt*
                    _do-actor-call _handle-await)])
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
                          (make-mactor:object
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
       (_$ (mactor:local-link-point-to mactor)
           args)]
      ;; Not a callable mactor!
      [_other
       (error 'not-callable
              "Not an encased or object mactor:" mactor)]))

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
                        (make-mactor:object initial-behavior
                                            become-unsealer become-sealed?))
         actor-refr)]
      ;; If someone returns another actor, just let that be the actor
      [(? live-refr? pre-existing-refr)
       pre-existing-refr]
      [_
       (error 'invalid-actor-handler "Not a procedure or live refr:" initial-behavior)]))

  (define (spawn-mactor mactor debug-name)
    (actormap-spawn-mactor! actormap mactor debug-name))

  (define (fulfill-promise promise-id sealed-val)
    (call/ec
     (lambda (return-early)
       (define orig-mactor
         (actormap-ref-or-die promise-id))
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
             ['() _void]
             ;; TODO: add support for <questioned> here, right?!?!
             [((? message? msg) rest-waiting ...)
              (let ((resolve-me (message-resolve-me msg))
                    (args (message-args msg)))
                ;; preserve FIFO by recursing first
                (send-rest rest-waiting)
                ;; and then send this message along
                (_send-message resolve-to-val resolve-me args))])))

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
               (break-promise promise-id
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
                           (_spawn ^resolver (list promise-id new-resolver-sealer)
                                   '^resolver)])
              ;; Now subscribe to the promise...
              (_send-listen resolve-to-val new-resolver #t)
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
                                 (_<-np (listener-info-resolve-me listener-info)
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
                     (_<-np (listener-info-resolve-me listener-info)
                            (list 'fulfill resolve-to-val)))
                   orig-listeners)))))

  ;; TODO: Add support for broken-because-of-network-partition support
  ;;   even for mactor:remote-link
  (define (break-promise promise-id sealed-problem)
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
                   (_<-np listener (list 'break problem)))
                 all-interested-listeners)
       ;; Now we "become" broken with that problem
       (actormap-set! actormap promise-id
                      (make-mactor:broken problem))]
      [(? mactor:remote-link?)
       (error "TODO: Implement breaking on captp disconnect!")]
      [#f (error "no actor with this id")]
      [_ (error "can only resolve eventual references")]))

  ;; Note that _handle-message is really, seriously for handling *toplevel*
  ;; messages... ie, turns.
  ;; This is the bulk of what's called and handled by actormap-turn-message.
  ;; (As opposed to actormap-turn*, which only supports calling, this also
  ;; handles any toplevel invocation of an actor, probably via message send.)
  (define (_handle-message msg)
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
                 (_<-np resolve-me (list 'fulfill call-result)))
               call-result)
             #;(with-exception-handler handle-exn
             do-call
             #:unwind? #t
             #:unwind-for-type #t)
             (do-call))]
          [orig-mactor (actormap-ref-or-die to-refr)])
      (match orig-mactor
        ;; If it's callable, we just use the call behavior, because
        ;; that's effectively the same code we'd be running anyway.
        ;; However, we do want to handle the resolution.
        [(or (? mactor:object?)
             (? mactor:encased?))
         (call-with-resolution
          (lambda () (_$ to-refr args)))]
        [(? mactor:local-link?)
         (let ((point-to (mactor:local-link-point-to orig-mactor)))
           (cond
            [(near-refr? point-to)
             (call-with-resolution
              (lambda () (_$ point-to args)))]
            ;; it's not near so we need to pass this along
            [else
             (_send-message point-to resolve-me args)
             _void]))]
        [(? mactor:broken?)
         (_<-np resolve-me (list 'break (mactor:broken-problem orig-mactor)))
         _void]
        [(? mactor:remote-link?)
         (let ([point-to (mactor:remote-link-point-to orig-mactor)])
           (call-with-resolution
            (lambda ()
              ;; Pass along the message.
              ;; Only produce a promise if we have a resolver.
              ((if resolve-me _<- _<-np) point-to args))))]
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
              (_handle-message (make-message vat-connector point-to resolve-me args))]
             [else
              ;; Otherwise, we need to forward this message to the appropriate
              ;; vat
              (_send-message point-to resolve-me args)
              _void])]
           ;; But if it's a remote promise then we queue it in the waiting
           ;; messages because we prefer to have messages "swim as close
           ;; as possible to the machine barrier where possible", with
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
            _void])]
        ;; Similar to the above w/ remote promises, except that we really
        ;; just don't know where things go *at all* yet, so no swimming
        ;; occurs.
        [(? mactor:naive?)
         (let ((unresolved (mactor-get-m~unresolved orig-mactor))
               (waiting-messages (mactor:naive-waiting-messages orig-mactor)))
           (actormap-set! actormap to-refr
                          (make-mactor:naive unresolved
                                             (cons msg waiting-messages)))
           _void)]
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
                             (_spawn-promise-values #:question-finder
                                                    followup-question-finder
                                                    #:captp-connector
                                                    captp-connector)])
                (queue-new-msg! (make-forward-to-captp
                                 (make-questioned (make-message vat-connector
                                                                to-question-finder
                                                                followup-question-resolver
                                                                args)
                                                  followup-question-finder)
                                 captp-connector))
                followup-question-promise)]
             ;; Otherwise, we can just send it without any question and return
             ;; void
             [else
              (queue-new-msg! (make-forward-to-captp
                               (make-message vat-connector to-question-finder #f args)
                               captp-connector))
              _void])))])))

  ;; helper to the below two methods
  (define* (_send-message to-refr resolve-me args
                          #:key [answer-this-question #f])
    (unless (live-refr? to-refr)
      (error 'send-message
             "Don't know how to send a message to:" to-refr))
    (let* ((base-message (make-message vat-connector to-refr resolve-me args))
           (new-message
            (if answer-this-question
                (make-questioned base-message answer-this-question)
                base-message)))
      (queue-new-msg! new-message)))

  (define (_<-np to-refr args)
    (_send-message to-refr #f args)
    _void)

  ;; Well, this does do a bit more heavy lifting than *just* call
  ;; _send-message.
  ;;
  ;; It also constructs a promise (including, possibly, a question promise)
  (define (_<- to-refr args)
    (match to-refr
      [(? local-refr?)
       (let-values ([(promise resolver)
                     (_spawn-promise-values)])
         (_send-message to-refr resolver args)
         promise)]
      [(? remote-refr?)
       (let*-values (((captp-connector)
                      (remote-refr-captp-connector to-refr))
                     ((question-finder)
                      (captp-connector 'new-question-finder))
                     ((promise resolver)
                      (_spawn-promise-values #:question-finder
                                             question-finder
                                             #:captp-connector
                                             captp-connector)))
         (_send-message to-refr resolver args
                        #:answer-this-question question-finder)
         promise)]
      [to-refr
       (error 'send-message
              "Don't know how to send a message to:" to-refr)]))

  (define* (_send-listen to-refr listener #:optional [wants-partial? #f])
    (match to-refr
      [(? live-refr?)
       (let ([listen-req
              (make-listen-request vat-connector to-refr listener wants-partial?)])
         (set! new-msgs (cons listen-req new-msgs)))]
      [val (<-np listener 'fulfill val)]))

  (define (_handle-listen to-refr listener wants-partial?)
    #;(define (handle-exn err)
      (when display-or-log-error
        (display-or-log-error err while-handling-listen-header))
      `#(fail ,err))
    (define (do-call)
      (unless (near-refr? to-refr)
        (error 'not-a-near-refr "Not a near refr:" to-refr))
      (define mactor
        (actormap-ref-or-die to-refr))
      (match mactor
        [(? mactor:local-link?)
         (let ((point-to
                (mactor:local-link-point-to mactor)))
           (if (near-refr? point-to)
               (_handle-listen (mactor:local-link-point-to mactor)
                               listener wants-partial?)
               (_send-listen point-to listener wants-partial?)))]
        ;; This object is a local promise, so we should handle it.
        [(? mactor:unresolved?)
         ;; Set a new version of the local-promise with this
         ;; object as a listener
         (actormap-set! actormap to-refr
                        (mactor:unresolved-add-listener mactor listener
                                                        wants-partial?))]
        ;; In the following cases we can resolve the listener immediately...
        [(? mactor:broken? mactor)
         (_<-np listener (list 'break (mactor:broken-problem mactor)))]
        [(? mactor:encased? mactor)
         (_<-np listener (list 'fulfill (mactor:encased-val mactor)))]
        [(? mactor:object? mactor)
         (_<-np listener (list 'fulfill to-refr))]
        ;; For remote links, we resolve directly to that reference
        [(? mactor:remote-link? mactor)
         (_<-np listener (list 'fulfill (mactor:remote-link-point-to mactor)))])
      _void)
    #;(with-exception-handler handle-exn
      do-call
      #:unwind? #t
      #:unwind-for-type #t)
    (do-call))

  ;; At THIS stage, fulfilled-handler, broken-handler, finally-handler should
  ;; be actors or #f.  That's not the case in the user-facing
  ;; `on' procedure.
  (define* (_on on-refr fulfilled-handler broken-handler finally-handler promise?)
    (define-values (return-promise return-p-resolver)
      (if promise?
          (spawn-promise-values)
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
                 (syscaller 'send-message
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
         _void]
        [('break problem)
         (handle-broken problem)
         _void]))
    (define listener
      (_spawn ^on-listener '() '^on-listener))
    (_send-listen on-refr listener)
    (when promise?
      return-promise))

  ;; TODO: We only really seem to need/use new-msgs now, so simplify
  ;; to just hand that back.
  (define (get-internals)
    (list actormap new-msgs))

  (define (set-closed! val)
    (set! closed? val))

  (values this-syscaller get-internals set-closed!))

(define (call-with-fresh-syscaller am proc)
  (define-values (sys get-sys-internals set-closed!)
    (fresh-syscaller am))
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
      (set-closed! #f))
    (lambda ()
      (parameterize ([current-syscaller sys])
        (proc sys get-sys-internals)))
    (lambda ()
      (set-closed! #t))))

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



;; Core API (spawn, $, <-, <-np, on)
;; =================================

;; System calls
(define (spawn constructor . args)
  "Construct and return a reference to the actor described by
CONSTRUCTOR, passing it ARGS.

Type: Constructor Any ... -> Actor"
  (define sys (get-syscaller-or-die))
  (sys 'spawn constructor args (procedure-name constructor)))
(define (spawn-named name constructor . args)
  "Construct and return a reference to an actor with the debug name
NAME described by CONSTRUCTOR, passing it ARGS.

Type: Symbol Constructor Any ... -> Actor"
  (define sys (get-syscaller-or-die))
  (sys 'spawn constructor args name))
(define ($ refr . args)
  "Synchronously invoke REFR with ARGS; return the result.

Type: Actor Any ... -> Any"
  (define sys (get-syscaller-or-die))
  (sys '$ refr args))
(define (<- refr . args)
  "Asynchronously invoke REFR with ARGS; return a promise.

Type: Actor Any ... -> Promise"
  (define sys (get-syscaller-or-die))
  (sys '<- refr args))
(define (<-np refr . args)
  "Asynchronously invoke REFR with ARGS; return nothing.

Type: Actor Any ... -> Void"
  (define sys (get-syscaller-or-die))
  (sys '<-np refr args))

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
       _void)]
    [(? remote-refr?)
     (let ((captp-connector (remote-refr-captp-connector to-refr)))
       (captp-connector 'handle-message
                        (make-message captp-connector to-refr #f args))
       _void)]))

;; Listen to a promise
(define* (listen-to to-refr listener #:key [wants-partial? #f])
  "Wait for TO-REFR to resolve then inform LISTENER. If WANTS-PARTIAL?
is #t, return updates rather than waiting for full promise resolution.
Return nothing.

Type: Promise Actor -> Void"
  (define sys (get-syscaller-or-die))
  (sys 'send-listen to-refr listener wants-partial?))

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
  (sys 'on vow (maybe-actorize fulfilled-handler 'fulfilled-handler)
       (maybe-actorize broken-handler 'broken-handler)
       (maybe-actorize finally-handler 'finally-handler)
       promise?))



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
     (sys 'fulfill-promise promise (sealer val))
     (bcom already-resolved)]
    [('break problem)
     (define sys (get-syscaller-or-die))
     (sys 'break-promise promise (sealer problem))
     (bcom already-resolved)]))

(define* (_spawn-promise-values #:key
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
    (sys 'spawn-mactor
         (if question-finder
             (begin
               (unless captp-connector
                 (error 'question-finder-without-captp-connector))
               (make-mactor:question m-unresolved
                                     captp-connector
                                     question-finder))
             (make-mactor:naive m-unresolved '()))
         #f))
  (define resolver
    (spawn ^resolver promise sealer))
  (values promise resolver))

;; We don't want to expose the keyword arguments of the parent
;; procedure to just everyone, hence this indirection
(define (spawn-promise-values)
  "Return a promise and its associated resolver as a values object.

Type: -> (Values Promise Resolver)"
  (_spawn-promise-values))

;; Convenient, sometimes
(define (spawn-promise-cons)
  "Return a promise and its associated resolver as a cons pair.

Type: -> (Promise . Resolver)"
  (call-with-values spawn-promise-values cons))



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
                      (make-mactor:object actor-handler
                                          become-unseal become?))
       actor-refr)]
    [(? live-refr? pre-existing-refr)
     pre-existing-refr]
    [_
     (error 'invalid-actor-handler "Not a procedure or live refr:" actor-handler)]))

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
   (lambda (sys get-sys-internals)
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
  (define-values (returned-val transactormap _nm)
    (actormap-turn* actormap to-refr args))
  returned-val)

;; like actormap-run but also returns the new actormap, new-msgs
(define (actormap-run* actormap thunk)
  (define-values (actor-refr new-actormap)
    (actormap-spawn (make-transactormap actormap) (lambda (bcom) thunk)))
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


(define while-handling-header
  "While attempting to handle message")
(define before-even-able-to-handle-header
  "Before even being able to handle message")
(define while-handling-listen-header
  "While handling listen request")

(define (display-backtrace* stack)
  ;; When displaying a backtrace in fibers, it's possible that
  ;; terminal-width in (system repl debug) will throw an error trying
  ;; to call (string->number #f) because the COLUMNS environment
  ;; variable isn't set.  We're not entirely sure why this happens,
  ;; but to work around it we set COLUMNS to Guile's own default of 72
  ;; if it hasn't been set already.
  (unless (getenv "COLUMNS")
    (setenv "COLUMNS" "72"))
  ;; Specify stack frame range explicitly, otherwise display-backtrace
  ;; will display additional frames in the current stack for some
  ;; reason!
  (display-backtrace stack (current-error-port) 0 (stack-length stack))
  (newline (current-error-port)))

(define (simple-display-error msg err stack)
  (newline (current-error-port))
  (display ";; === Caught error: ===\n" (current-error-port))
  (format (current-error-port) ";;  message: ~s\n" msg)
  (format (current-error-port) ";;  exception: ~s\n" err)
  (display-backtrace* stack))

(define (make-no-op msg)
  (lambda _ _void))

(define &actormap-turn-error
  (make-exception-type '&actormap-turn-error &error '(stack)))

(define make-actormap-turn-error (record-constructor &actormap-turn-error))

(define actormap-turn-error-stack
  (exception-accessor &actormap-turn-error
                      (record-accessor &actormap-turn-error 'stack)))

(define* (actormap-turn-message actormap msg
                                #:key
                                [error-handler simple-display-error]
                                [reckless? #f]
                                [catch-errors? #t])
  ;; TODO: Kuldgily reimplements part of actormap-turn*... maybe
  ;; there's some opportunity to combine things, dunno.
  (call-with-fresh-syscaller
   (if reckless?
       actormap
       (make-transactormap actormap))
   (lambda (sys get-sys-internals)
     (define (error-prompt-handler kont err stack-at-exn)
       ;; Since we threw an exception, we should inform that this
       ;; failed... if anyone cares
       (define resolve-me
         (message-who-wants-response msg))
       (define new-msgs
         (if resolve-me
             (list (make-message (sys 'vat-connector) resolve-me #f (list 'break err)))
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
         (make-stack #t ; get the current stack
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
            (sys 'handle-message msg)]
           [(? listen-request? lr)
            (sys 'handle-listen
                 (listen-request-to lr)
                 (listen-request-listener lr)
                 (listen-request-wants-partial? lr))]))
       (match (get-sys-internals)
         [(new-actormap new-msgs)
          (values `#(ok ,result) new-actormap new-msgs)]))
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
