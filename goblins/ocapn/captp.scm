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

(define-module (goblins ocapn captp)
  #:use-module ((goblins core) #:renamer (lambda (x) (if (eq? x '$) '$C x)))
  #:use-module (goblins vat)
  #:use-module (goblins ocapn define-recordable)
  #:use-module (goblins ocapn structs-urls)
  #:use-module (goblins actor-lib methods)
  #:use-module (ice-9 match)
  #:use-module (syrup)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:use-module (goblins ocapn crypto-funcs)
  )

;; This should be better documented, and will when it becomes more of
;; a "standardized protocol" as opposed to a "bespoke implementation".
;;
;; Much of CapTP here based on E's docs:
;;   http://erights.org/elib/distrib/captp/index.html
;; and capnproto's writeups:
;;   https://capnproto.org/rpc.html
;;   https://github.com/sandstorm-io/capnproto/blob/master/c++/src/capnp/rpc.capnp
;;
;; For the gory details of "Chris figuring out how CapTP works"
;; see these monster threads:
;;   https://groups.google.com/g/cap-talk/c/xWv2-J62g-I
;;   https://groups.google.com/g/cap-talk/c/-JYtc-L9OvQ
;;
;; For the handoff stuff:
;;   https://dustycloud.org/tmp/captp-handoff-musings.org.txt
;;   https://dustycloud.org/misc/3vat-handoff-scaled.jpg

(define-record-type <captp-session-severed>
  (captp-session-severed)
  captp-session-severed?)

;;; Messages

(define-recordable op:bootstrap
  (answer-pos resolve-me-desc))


;; Queue a delivery of verb(args..) to recip, discarding the outcome.
(define-recordable op:deliver-only
  (;; Position in the table for the target
   ;; (sender's imports, reciever's exports)
   to-desc
   ;; Either the method name, or #f if this is a procedure call
   method
   ;; Either arguments to the method or to the procedure, depending
   ;; on whether method exists
   args
   kw-args))

;; Queue a delivery of verb(args..) to recip, binding answer/rdr to the outcome.
(define-recordable op:deliver
  (to-desc
   method
   args
   kw-args
   answer-pos
   resolve-me-desc))  ; a resolver, probably an import (though it could be a handoff)

(define-recordable op:abort
  (reason))

(define-recordable op:listen
  (to-desc listener-desc wants-partial?))

(define-recordable op:gc-export
  (export-pos wire-delta))

(define-recordable op:gc-answer
  (answer-pos))

(define-recordable desc:import-object
  (pos))

(define-recordable desc:import-promise
  (pos))

(define (desc:import-pos import-desc)
  (match import-desc
    [(? desc:import-object?)
     (desc:import-object-pos import-desc)]
    [(? desc:import-promise?)
     (desc:import-promise-pos import-desc)]))

(define (desc:import? obj)
  (or (desc:import-object? obj)
      (desc:import-promise? obj)))

;; Whether it's an import or export doesn't really matter as much to
;; the entity exporting as it does to the entity importing
(define-recordable desc:export
  (pos))

;; Something to answer that we haven't seen before.
;; As such, we need to set up both the promise import and this resolver/redirector
(define-recordable desc:answer
  (pos))

;; This is a general sig-envelope, we might have some more specific
;; ones.  Whatever signed must refer to another serializable record
;; which is concretely typed.  If not, we run into confused deputy,
;; replay, oracle attack possibilities.  See also:
;;   https://sandstorm.io/news/2015-05-01-is-that-ascii-or-protobuf
;; Note that the key is not referred to; if it isn't obvious by the
;; payload and the protocol, then we aren't doing things right.
(define-recordable desc:sig-envelope
  (signed signature))

;; Handoffs have three roles:
;;  - Gifter: who's sharing their import
;;  - Receiver: who's receiving the gift
;;  - Exporter: the location where the gift import is exported from
;;    (presumably, where it lives, though it may be a promise which
;;    eventually points to something else)

;; The handoff certificate from the gifter
(define-recordable desc:handoff-give
  (;; handoff signing key this is being given to
   ;;   : handoff-key?
   recipient-key
   ;; exporter-location(-hint(s)): how to connect to get this
   ;;   : ocap-machine-uri?
   ;;   Note that currently this requires a certain amount of VatTP
   ;;   crossover, since we have to give a way to connect to VatTP...
   exporter-location
   ;; session: which session betweein gifter and exporter at the location
   ;;   : bytes?
   session
   ;; gifter-side: which "named side" of the session is the gifter
   ;;   : bytes?
   gifter-side
   ;; gift-id: The gift id associated with this gift
   ;;   : (or/c integer? bytes?)
   gift-id))

;; TODO: Maybe we only need the receiving-side, unsure
(define-recordable desc:handoff-receive
  (receiving-session
   receiving-side
   handoff-count
   signed-give))

;; machinetp operations/descriptions
(define-recordable mtp:op:start-session
  (handoff-pubkey
   ;; a sig-envelope signed by handoff-pubkey with a <my-location $location-data>
   acceptable-location
   acceptable-location-sig))

;; Confirm we both have the session name, each side signs with its
;; respective key
;; Not sure this is necessary...
#;(define-recordable-struct mtp:op:confirm-session
  (session-name-sig)
  marshall::mtp:op:start-session unmarshall::mtp:op:start-session)

;; TODO: 3 vat/machine handoff versions (Promise3Desc, Far3Desc)

(define marshallers
  (list marshall::op:bootstrap
        marshall::op:deliver-only
        marshall::op:deliver
        marshall::op:abort
        marshall::op:listen
        marshall::op:gc-export
        marshall::op:gc-answer
        marshall::desc:import-object
        marshall::desc:import-promise
        marshall::desc:export
        marshall::desc:answer
        marshall::desc:sig-envelope
        marshall::desc:handoff-give
        marshall::desc:handoff-receive
        marshall::mtp:op:start-session

        marshall::ocapn-machine
        marshall::ocapn-sturdyref
        marshall::ocapn-cert
        marshall::ocapn-bearer-union))

(define unmarshallers
  (list unmarshall::op:bootstrap
        unmarshall::op:deliver-only
        unmarshall::op:deliver
        unmarshall::op:abort
        unmarshall::op:listen
        unmarshall::op:gc-export
        unmarshall::op:gc-answer
        unmarshall::desc:import-object
        unmarshall::desc:import-promise
        unmarshall::desc:export
        unmarshall::desc:answer
        unmarshall::desc:sig-envelope
        unmarshall::desc:handoff-give
        unmarshall::desc:handoff-receive
        unmarshall::mtp:op:start-session

        unmarshall::ocapn-machine
        unmarshall::ocapn-sturdyref
        unmarshall::ocapn-cert
        unmarshall::ocapn-bearer-union))

;; Doesn't verify that it's *valid*, just that it's *signed*
(define (signed-handoff-give? obj)
  (match obj
    [($ desc:sig-envelope (? desc:handoff-give? handoff-give-cert)
                          (? bytevector? sig))
     #t]
    [_ #f]))

;; TODO:

(define (signed-handoff-receive? obj)
  (match obj
    [($ desc:sig-envelope ($ desc:handoff-receive (? bytevector? session)
                                                  (? bytevector? session-side)
                                                  integer?
                                                  (? signed-handoff-give?))
                          (? bytevector? sig))
     #t]
    [_ #f]))


(define-record-type <internal-shutdown>
  (internal-shutdown reason)
  internal-shutdown?
  (reason internal-shutdown-reason))

;; Internal commands from the vat connector
(define-record-type <cmd-send-message>
  (cmd-send-message msg)
  cmd-send-message?
  (msg cmd-send-message-msg))

(define-record-type <cmd-send-listen>
  (cmd-send-listen to-refr listener-refr wants-partial?)
  cmd-send-listen?
  (to-refr cmd-send-listen-to-refr)
  (listener-refr cmd-send-listen-listener-refr)
  (wants-partial? cmd-send-listen-wants-partial?))

(define-record-type <cmd-send-gc-answer>
  (cmd-send-gc-answer answer-pos)
  cmd-send-gc-answer?
  (answer-pos cmd-send-gc-answer-answer-pos))

(define-record-type <cmd-send-gc-export>
  (cmd-send-gc-export export-pos)
  cmd-send-gc-export?
  (export-pos cmd-send-gc-export-export-pos))

(define (setup-captp-conn send-to-remote
                          ;; coordinates between multiple captp connections:
                          ;; handoffs, etc.
                          coordinator
                          bootstrap-obj
                          intra-machine-warden intra-machine-incanter)
  ;; position sealers, so we know this really is from our imports/exports
  ;; @@: Not great protection, subject to a reuse attack, but really
  ;;   this is just an extra step... in general we shouldn't be exposing
  ;;   the refr internals to most users
  (define-values (pos-seal pos-unseal pos-sealed?)
    (make-sealer-triplet))
  (define-values (partition-seal partition-unseal partition-tm?)
    (make-sealer-triplet))

  ;; Question finders are a weird thing... we need some way to be able to
  ;; look up what question corresponds to an entry in the table.
  ;; Used by mactor:question (a special kind of promise),
  ;; since messages sent to a question are pipelined through the answer
  ;; side of some "remote" machine.
  (define-record-type <question-finder>
    (make-question-finder)
    question-finder?)

  (define (_handle-message msg)
    (match msg
      [(or (? message?) (? questioned?))
       (<-np-extern internal-handler
                    (cmd-send-message msg))]
      [($ listen-request to-refr listener wants-partial?)
       (<-np-extern internal-handler
                    (cmd-send-listen to-refr listener
                                     wants-partial?))])
    _void)

  (define (_partition-unsealer-tm-cons)
    (cons partition-unseal partition-tm?))

  (define (_listen-request to-refr listen-refr
                           #:wants-partial? [wants-partial? #f])
    (<-np-extern internal-handler
                 (cmd-send-listen to-refr listen-refr
                                  wants-partial?)))

  (define (^connector-obj _bcom)
    (define intra-machine-beh
      (methods
       [(get-handoff-privkey)
        ($C coordinator 'get-handoff-privkey)]
       [(get-remote-location)
        ($C coordinator 'get-remote-location)]
       [(get-remote-bootstrap)
        remote-bootstrap-vow]
       [(get-session-name)
        ($C coordinator 'get-session-name)]
       [(get-our-side-name)
        ($C coordinator 'get-our-side-name)]))
    (define main-beh
      (methods
       [(resolve-on-sever sever-resolver)
        (define (^cancel-sever-notification bcom)
          (define noop-beh
            (lambda () 'no-op))
          (lambda ()
            ($C interested-in-sever 'remove sever-resolver)
            (bcom noop-beh)))
        ($C interested-in-sever 'add sever-resolver)
        (spawn ^cancel-sever-notification)]
       [(cancel-sever-interest sever-resolver)
        ($C interested-in-sever 'remove sever-resolver)]))
    (ward intra-machine-warden intra-machine-beh
          #:extends main-beh))
  (define connector-obj (spawn ^connector-obj))
  (define (_get-connector-obj) connector-obj)

  (define (same-connection? refr)
    (and (remote-refr? refr)
         (eq? (remote-refr-captp-connector refr) captp-connector)))

  (define-simple-dispatcher captp-connector
    [handle-message _handle-message]
    [new-question-finder make-question-finder]
    [listen _listen-request]
    [partition-unsealer-tm-cons _partition-unsealer-tm-cons]
    [same-connection? same-connection?]
    ;; For all the things that we don't want thread stompiness on...
    [connector-obj _get-connector-obj])

  (define next-export-pos 0)
  (define next-question-pos 0)
  ;; (define next-promise-pos 0)

  (define exports-val2pos (make-hash-table))    ; (eq)  exports[val]:   chosen by us 
  (define exports-pos2val (make-hash-table))    ; (eqv) exports[pos]:   chosen by us
  ;; TODO: This doesn't make sense if the value isn't wrapped in a weak
  ;;   reference... I think this also needs to go in both directions to work
  ;;   from a GC perspective
  (define imports (make-hasheqv))               ; (eqv) imports:        chosen by peer
  (define questions (make-weak-hasheqv))        ; (eqv) questions:      chosen by us
  (define answers (make-hasheqv))               ; (eqv) answers:        chosen by peer

  ;; TODO: This should really be some kind of box that the other side
  ;;   can query, right?
  (define running? #t)

  ;; These are imports that we've processed when we already had allocated
  ;; a reference.  We batch send GC messages about these as appropriate.
  ;; Note that we say "spare" because there's one more count that's
  ;; associated with the reference itself.
  ;; Mapping of slot position -> count
  (define spare-import-counts
    (make-hash-table))  ; (eqv)
  ;; The inverse: tracking how many export numbers we've given so we can
  ;; know when it hits 0 and is ok to remove
  (define export-counts
    (make-hash-table))  ; (eqv)

  (define (increment-spare-imports-count! import-pos)
    (hashv-set! spare-import-counts import-pos
                (add1 (hashv-ref spare-import-counts import-pos 0))))
  ;; Go through all the "spare imports" and reset them
  (define (handle-spare-imports!)
    (for ([(import-pos count) (in-hash spare-import-counts)])
         ;; Send a gc-export message for this many
         (send-to-remote (op:gc-export import-pos count))
         ;; Reset these
         (hashv-remove! spare-import-counts import-pos)))
  (define (decrement-exports-count-maybe-remove! export-pos delta)
    (-> integer? integer? any/c)
    (match (hashv-ref export-counts export-pos #f)
      [(and (? integer?) (? positive? cur-count))
       (match (- cur-count delta)
         ;; time to remove
         [0
          (hashv-remove! export-counts export-pos)
          ;; Remove this export from both
          (let ([val (hashv-ref exports-pos2val export-pos)])
            (hashq-remove! exports-val2pos val)
            (hashv-remove! exports-pos2val export-pos))]
         ;; decremented but still positive
         [(and (? integer?) (? positive? new-count))
          (hashv-set! export-counts export-pos new-count)]
         [neg-count
          (error 'exports-gc-error
                 "Tried decrementing export-pos ~a by ~a but that's negative: ~a"
                 neg-count)])]
      [other-val
       (error 'exports-gc-error
              "Tried to decrement the exports count for position ~a but its value was ~a"
              export-pos other-val)]))

  ;; Now make the will executor and boot its corresponding thread
  ;; for cooperative GC.
  (define refr-will-executor
    (make-will-executor))

  ;; TODO: Should we move this out from a thread and put it in the
  ;;   main loop and run it after every loop with will-try-execute?
  ;;   That could reduce the chance of some race conditions, though
  ;;   I'm not sure it's strictly necessary.
  (syscaller-free-thread
   (lambda ()
     (let lp ()
       (will-execute refr-will-executor)
       (lp))))

  (define (make-question-will-handler question-pos)
    (lambda _
      ;; There's (I think?) a possible race condition here if we were to
      ;; use send-to-remote from right here, so we have the main thread
      ;; send it via the internal-ch
      ;; TODO: Oh fuck I broke that in commit 7f575d0d didn't I
      ;;   ... so that's why we didn't want to use a vat for this???
      (<-np-extern internal-handler (cmd-send-gc-answer question-pos))))
  (define (install-question-will-handler! question-finder question-pos)
    (will-register refr-will-executor question-finder
                   (make-question-will-handler question-pos)))

  (define (make-import-will-handler import-pos)
    (lambda _
      (hashv-remove! imports import-pos)
      (<-np-extern internal-handler (cmd-send-gc-export import-pos))))
  (define (install-import-will-handler! refr import-pos)
    (will-register refr-will-executor refr
                   (make-import-will-handler import-pos)))

  ;; Possibly install an export for this local refr, and return
  ;; this export id
  ;; TODO: we maybe need to differentiate between local-live-refr and
  ;;   remote-live-proxy-refr (once we set that up)?
  (define/contract (maybe-install-export! refr)
    (-> live-refr? any/c)  ; TODO: Maybe de-contract this and manually check for speed
    (cond
     ;; Already have it, no need to increment next-export-pos
     [(hashv-ref exports-val2pos refr)
      =>
      (lambda (export-pos)
        ;; However, we do need to increment our export count
        (match (hashv-ref export-counts export-pos #f)
          ;; Uh, we screwed up our bookkeeping at some point
          [#f
           (error 'no-export-count-wtf
                  "No export count for ~a" export-pos)]
          [cur-count
           (hashv-set! export-counts export-pos (add1 cur-count))])
        ;; now finally return the export position
        export-pos)]
     ;; Nope, let's export this
     [else
      (let ((export-pos next-export-pos))
        ;; get this export-pos and increment next-export-pos
        (set! next-export-pos (add1 export-pos))
        ;; install in both export tables
        (hashv-set! exports-pos2val export-pos
                    refr)
        (hashq-set! exports-val2pos refr
                    export-pos)
        ;; (sanity check:) make sure there's no export count currently
        (when (hashv-ref export-counts export-pos)
          (error 'shouldnt-be-export-count-wtf
                 "Adding a new export but there was already an export count for pos: ~a"
                 export-pos))
        ;; and set the export count to 1
        (hashv-set! export-counts export-pos 1)
        export-pos)]))

  (define/contract (marshall-local-refr! local-refr)
    (-> local-refr? (or/c desc:import-object
                          desc:import-promise))
    (define export-pos
      (maybe-install-export! local-refr))
    (match local-refr
      [(? local-object?)
       (desc:import-object export-pos)]
      [(? local-promise?)
       (desc:import-promise export-pos)]))

  (define (maybe-install-import! import-desc)
    (define import-pos
      (desc:import-pos import-desc))
    (define (install-new-import!)
      ;; construct the new reference...
      (define new-refr
        (match import-desc
          [(? desc:import-object?)
           (make-remote-object-refr captp-connector
                                    (pos-seal import-pos))]
          [(? desc:import-promise?)
           (make-remote-promise-refr captp-connector
                                     (pos-seal import-pos))]))
      ;; Install it...
      (hashv-set! imports import-pos (make-weak-box new-refr))
      ;; set up the will handler...
      (install-import-will-handler! new-refr import-pos)
      ;; and return it.
      new-refr)
    (cond
     [(hashv-ref imports import-pos)
      =>
      (lambda (import-box)
        ;; Oh, we've already got that.  Reference and return it.
        (match (weak-box-value import-box)
          ;; Possible race condition: Apparently it was GC'ed
          ;; mid-operation so now we need to add it back
          ;; @@: *sweating profusely* but is this all the possible
          ;;     race conditions???
          [#f (install-new-import!)]
          ;; looks like we got the refr, return as-is
          [refr
           (increment-spare-imports-count! import-pos)
           refr]))]
     [else
      (install-new-import!)]))

  (define/contract (question-finder->question-pos! question-finder)
    (-> question-finder? integer?)
    (cond
     ;; we already have a question relevant to this question id
     ((hashq-ref questions question-finder) => identity)
     (else
      ;; new question id...
      (let ([question-pos next-question-pos])
        ;; install our question at this question id
        (hashq-set! questions question-finder question-pos)
        (install-question-will-handler! question-finder question-pos)
        ;; increment the next-question id
        (set! next-question-pos (add1 next-question-pos))
        ;; and return the question-pos we set up
        question-pos))))

  ;; general argument marshall/unmarshall for import/export

  ;; TODO: need to handle lists/dotted-lists/vectors
  (define (outgoing-pre-marshall! obj)
    (match obj
      [(obj ...)
       (map outgoing-pre-marshall! obj)]
      [(? hash?)
       
       (for/fold ([ht #hash()])
                 ([(key val) obj])
                 (hash-set ht (outgoing-pre-marshall! key)
                           (outgoing-pre-marshall! val)))]
      [(? set?)
       (for/set ([x obj])
                (outgoing-pre-marshall! x))]
      [(? local-promise?)
       (desc:import-promise (maybe-install-export! obj))]
      [(? local-object?)
       (desc:import-object (maybe-install-export! obj))]
      [(? remote-refr?)
       (define refr-captp-connector
         (remote-refr-captp-connector obj))
       (cond
        ;; from this captp
        [(eq? refr-captp-connector captp-connector)
         (desc:export (pos-unseal (remote-refr-sealed-pos obj)))]
        ;; elsewhere, let the coordinator do it
        [else
         ($C coordinator 'make-handoff-base-cert obj)])]
      [(? void?)
       (make-syrec* 'void)]
      ;; TODO: Supply more machine-crossing exception types here
      [(? exn:fail?)
       (make-syrec* 'exn:fail:mystery)]
      ;; And here's the general-purpose record that users can use
      ;; for whatever purpose is appropriate
      [($ <syrec> record-tag record-args)
       (make-syrec* 'user-record record-tag record-args)]
      [_ obj]))

  (define (incoming-post-unmarshall! obj)
    (match obj
      [(obj ...)
       (map incoming-post-unmarshall! obj)]
      [(? hash?)
       (for/fold ([ht #hash()])
                 ([(key val) obj])
                 (hash-set ht (incoming-post-unmarshall! key)
                           (incoming-post-unmarshall! val)))]
      [(? set?)
       (for/set ([x obj])
                (incoming-post-unmarshall! x))]
      [(or (? desc:import-promise?) (? desc:import-object?))
       (maybe-install-import! obj)]
      [(desc:export pos)
       (hashv-ref exports-pos2val pos)]
      [($ <syrec> 'exn:fail:mystery '())
       (make-mystery-fail)]
      [($ <syrec> 'void '())
       _void]
      ;; unserialize user-defined records
      [($ <syrec> 'user-record (list record-tag record-args))
       (make-syrec record-tag record-args)]
      [($ <syrec> unknown-record-tag record-args)
       (error 'captp-unknown-record-rag "Unknown record tag: ~a"
              unknown-record-tag)]
      [(? signed-handoff-give? sig-envelope-and-handoff)
       ;; We need to send this message to the coordinator, which will
       ;; work with the machine to (hopefully) get it to the right
       ;; destination
       (define handoff-vow
         ($C coordinator 'start-retrieve-handoff sig-envelope-and-handoff))
       handoff-vow]
      [_ obj]))

  (define (unmarshall-to-desc to-desc)
    (match to-desc
      [($ desc:export export-pos)
       (hashv-ref exports-pos2val export-pos)]
      [($ desc:answer answer-pos)
       (hashv-ref answers answer-pos)]))

  (define (marshall-to obj)
    (match obj
      [(? question-finder?)
       (make-desc:answer (hashq-ref questions obj))]
      [(? remote-refr?)
       (define refr-captp-connector
         (remote-refr-captp-connector obj))
       (cond
        ;; from this captp
        [(eq? refr-captp-connector captp-connector)
         (desc:export (pos-unseal (remote-refr-sealed-pos obj)))]
        [else
         (error 'captp-to-wrong-machine)])]))

  (define (install-answer! answer-pos resolve-me-desc)
    (define resolve-me
      (maybe-install-import! resolve-me-desc))
    (when (hashv-ref answers answer-pos)
      (error 'already-have-answer
             "~a" answer-pos))
    (match-let (((answer-promise . answer-resolver)
                 (spawn-promise-cons)))
      (hashv-set! answers answer-pos answer-promise)
      (listen answer-promise resolve-me)
      (values answer-promise answer-resolver)))

  ;; Resolvers that are interested in when this poops out
  (define interested-in-sever
    (spawn ^seteq))

  (define (tear-it-down shutdown-type reason)
    (set! exports-val2pos #f)
    (set! exports-pos2val #f)
    (set! imports #f)
    (set! questions #f)
    (set! answers #f)
    (set! running? #f)
    (for ([interested ($C interested-in-sever 'data)])
         (<-np interested 'fulfill (list 'severed shutdown-type
                                         reason)))
    (set! interested-in-sever #f))

  (define (abort-because reason)
    (send-to-remote (op:abort reason))
    (tear-it-down))

  ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ;; TODO TODO TODO: EACH of these needs to call (handle-spare-imports!)
  ;; at the end of its behavior!  Probably the best thing to do is to
  ;; make a wrapper for each of these that does so... and also which
  ;; adds an error handler which aborts the whole thing if such an
  ;; error occurs
  ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  (define ((^captp-incoming-handler bcom) msg)
    (unless running?
      (error 'captp-breakage "Captp session is no longer running but got ~a"
             msg))
    (match msg
      [($ op:bootstrap (? integer? answer-pos) resolve-me-desc)
       (let-values (((_answer-promise answer-resolver)
                     (install-answer! answer-pos resolve-me-desc)))
         ;; And since we're bootstrapping, we resolve it immediately
         ($C answer-resolver 'fulfill bootstrap-obj)
         _void)]
      ;; TODO: Handle case where the target doesn't exist?
      ;;   Or maybe just generally handle unmarshalling errors :P
      [($ op:deliver-only to-desc method
                        args-marshalled
                        kw-args-marshalled)
       ;; TODO: support distinction between method sends and procedure sends
       (define args
         (incoming-post-unmarshall! args-marshalled))
       (define kw-args
         (incoming-post-unmarshall! kw-args-marshalled))
       (define target (unmarshall-to-desc to-desc))
       (define-values (kws kw-vals)
         (kws-hasheq->kws-lists kw-args))
       (keyword-apply <-np kws kw-vals
                      target args)
       _void]
      [(op:deliver to-desc method
                   args-marshalled
                   kw-args-marshalled
                   answer-pos
                   resolve-me-desc)
       (define-values (_answer-promise answer-resolver)
         (install-answer! answer-pos resolve-me-desc))

       ;; TODO: support distinction between method sends and procedure sends
       (define args
         (incoming-post-unmarshall! args-marshalled))
       (define kw-args
         (incoming-post-unmarshall! kw-args-marshalled))
       (define target (unmarshall-to-desc to-desc))
       (define-values (kws kw-vals)
         (kws-hasheq->kws-lists kw-args))
       (define sent-promise
         (keyword-apply <- kws kw-vals target args))
       ($C answer-resolver 'fulfill sent-promise)
       _void]

      ;; TODO: Here's where we have to record that a listening interest
      ;; has occured, assuming we do the "automatically notify on session
      ;; severance" thing?
      ;;
      ;; Which means we'll also have to track incoming resolutions to
      ;; this promise somehow...?
      ;;
      ;; Actually the easiest thing to do here would be to create our own
      ;; promise-resolver pair, right here, at the captp perimeter, which
      ;; pipelines the result.
      [(op:listen (? desc:export? to-desc)
                  (? desc:import? listener-desc)
                  (? boolean? wants-partial?))
       (define to-refr
         (unmarshall-to-desc to-desc))
       (define listener
         (incoming-post-unmarshall! listener-desc))
       (listen to-refr listener
               #:wants-partial? wants-partial?)
       _void]
      [(op:gc-answer answer-pos)
       (hashv-remove! answers answer-pos)]
      [(op:gc-export (? integer? export-pos) (? integer? wire-delta))
       (decrement-exports-count-maybe-remove! export-pos wire-delta)]
      [(op:abort reason)
       (tear-it-down 'abort reason)]
      [(internal-shutdown reason)
       (tear-it-down 'internal-shutdown reason)]
      [other-message
       (error 'invalid-message "~a" other-message)]))

  (define ((^internal-handler bcom) cmd)
    (define (running-handle-cmd cmd)
      (match cmd
        [($ cmd-send-message msg)
         (define-values (real-msg answer-pos)
           (match msg
             [(? message?)
              (values msg #f)]
             [(questioned msg answer-this-question)
              (values msg (question-finder->question-pos! answer-this-question))]))
         (match-define (message to resolve-me kws kw-vals args)
                       real-msg)
         (define deliver-msg
           (if resolve-me
               (op:deliver (marshall-to to)
                           #;(desc:import (maybe-install-export! to))
                           #f ;; TODO: support methods
                           ;; TODO: correctly marshall everything here
                           (outgoing-pre-marshall! args)
                           (outgoing-pre-marshall!
                            (kws-lists->kws-hasheq kws kw-vals))
                           answer-pos
                           (marshall-local-refr! resolve-me))
               (op:deliver-only (marshall-to to)
                                #f ;; TODO: support methods
                                (outgoing-pre-marshall! args)
                                (outgoing-pre-marshall!
                                 (kws-lists->kws-hasheq kws kw-vals)))))
         (send-to-remote deliver-msg)]
        [($ cmd-send-listen (? remote-refr? to-refr) (? local-refr? listener-refr)
                            (? boolean? wants-partial?))
         (define listen-msg
           (op:listen (marshall-to to-refr)
                      (outgoing-pre-marshall! listener-refr)
                      wants-partial?))
         (send-to-remote listen-msg)]
        [($ cmd-send-gc-answer (? integer? answer-pos))
         (send-to-remote (op:gc-answer answer-pos))]
        [($ cmd-send-gc-export (? integer? export-pos))
         (send-to-remote (op:gc-export export-pos 1))]))
    (define (broken-handle-cmd cmd)
      (match cmd
        [($ cmd-send-message msg)
         (match-define (message to resolve-me kws kw-vals args)
                       msg)
         (when resolve-me
           (<-np resolve-me 'break (captp-session-severed)))]
        [($ cmd-send-listen (? remote-refr? to-refr) (? local-refr? listener-refr)
                          (? boolean? wants-partial?))
         (<-np listener-refr 'break (captp-session-severed))]
        [($ cmd-send-gc-answer (? integer? answer-pos))
         'no-op]
        [($ cmd-send-gc-export (? integer? export-pos))
         'no-op]))
    (if running?
        (running-handle-cmd cmd)
        (broken-handle-cmd cmd)))

  (define captp-incoming-handler
    (spawn ^captp-incoming-handler))
  (define internal-handler
    (spawn ^internal-handler))

  ;;; BEGIN REMOTE BOOTSTRAP OPERATION
  ;;; ================================
  (define this-question-finder
    (question-finder))
  ;; called for its effect of installing the question
  (question-finder->question-pos! this-question-finder)
  (define-values (remote-bootstrap-vow remote-bootstrap-resolver)
    (_spawn-promise-values #:question-finder
                           this-question-finder
                           #:captp-connector
                           captp-connector))
  (define bootstrap-msg
    (op:bootstrap (hashq-ref questions this-question-finder)
                  (outgoing-pre-marshall! remote-bootstrap-resolver)))
  (send-to-remote bootstrap-msg)
  ;;; END REMOTE BOOTSTRAP OPERATION
  ;;; ==============================

  (values captp-incoming-handler remote-bootstrap-vow))
