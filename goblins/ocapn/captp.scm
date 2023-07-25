;;; Copyright 2019-2021 Christine Lemmer-Webber
;;; Copyright 2023 David Thompson
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
  #:use-module ((fibers) #:select (spawn-fiber))
  #:use-module ((fibers timers) #:select (sleep))
  #:use-module ((goblins core) #:renamer (lambda (x) (if (eq? x '$) '$C x)))
  #:use-module (goblins vat)
  #:use-module (goblins ghash)
  #:use-module (goblins inbox)
  #:use-module (goblins ocapn marshalling)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib nonce-registry)
  #:use-module (goblins actor-lib swappable)
  #:use-module (goblins actor-lib ward)
  #:use-module (goblins utils assert-type)
  #:use-module (goblins utils simple-dispatcher)
  #:use-module (goblins utils simple-sealers)
  #:use-module (goblins utils bytes-stuff)
  #:use-module (goblins utils crypto)
  #:use-module (goblins contrib syrup)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (fibers channels)
  #:use-module ((gcrypt pk-crypto) #:prefix gcrypt:pk-crypto:)
  #:export (spawn-mycapn))

;;; Some crap to make this work in the port from Racket->Guile

(define local-promise? local-promise-refr?)
(define local-object? local-object-refr?)
(define add1 1+)
;; Old hack to get the "unspecified/undefined type"
(define _void (if #f #f))
(define (void? x) (eq? x _void))

(define _spawn-promise-values
  (@@ (goblins core) _spawn-promise-values))

(define captp-version "goblins-0.11")


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
(define-record-type <op:bootstrap>
  (op:bootstrap answer-pos resolve-me-desc)
  op:bootstrap?
  (answer-pos op:bootstrap-answer-pos)
  (resolve-me-desc op:bootstrap-resolve-me-desc))

(define-values (marshall::op:bootstrap unmarshall::op:bootstrap)
  (make-marshallers <op:bootstrap> #:name 'op:bootstrap))

;; Queue a delivery of verb(args..) to recip, discarding the outcome.
(define-record-type <op:deliver-only>
  (op:deliver-only to-desc args)
  op:deliver-only?
  ;; Position in the table for the target
  ;; (sender's imports, reciever's exports)
  (to-desc op:deliver-only-to-desc)
   ;; Either arguments to the method or to the procedure, depending
   ;; on whether method exists
  (args op:deliver-only-args))

(define-values (marshall::op:deliver-only unmarshall::op:deliver-only)
  (make-marshallers <op:deliver-only> #:name 'op:deliver-only))

;; Queue a delivery of verb(args..) to recip, binding answer/rdr to the outcome.
(define-record-type <op:deliver>
  (op:deliver to-desc args answer-pos resolve-me-desc)
  op:deliver?
  (to-desc op:deliver-to-desc)
  (args op:deliver-args)
  (answer-pos op:deliver-answer-pos)
  ;; a resolver, probably an import (though it could be a handoff)
  (resolve-me-desc op:deliver-resolve-me-desc))
(define-values (marshall::op:deliver unmarshall::op:deliver)
  (make-marshallers <op:deliver> #:name 'op:deliver))

(define-record-type <op:abort>
  (op:abort reason)
  op:abort?
  (reason op:abort-reason))
(define-values (marshall::op:abort unmarshall::op:abort)
  (make-marshallers <op:abort> #:name 'op:abort))

(define-record-type <op:listen>
  (op:listen to-desc listener-desc wants-partial?)
  op:listen?
  (to-desc op:listen-to-desc)
  (listener-desc op:listen-listener-desc)
  (wants-partial? op:listen-wants-partial?))
(define-values (marshall::op:listen unmarshall::op:listen)
  (make-marshallers <op:listen> #:name 'op:listen))

(define-record-type <op:gc-export>
  (op:gc-export export-pos wire-delta)
  op:gc-export?
  (export-pos op:gc-export-export-pos)
  (wire-delta op:gc-export-wire-delta))
(define-values (marshall::op:gc-export unmarshall::op:gc-export)
  (make-marshallers <op:gc-export> #:name 'op:gc-export))

(define-record-type <op:gc-answer>
  (op:gc-answer answer-pos)
  op:gc-answer?
  (answer-pos op:gc-answer-answer-pos))
(define-values (marshall::op:gc-answer unmarshall::op:gc-answer)
  (make-marshallers <op:gc-answer> #:name 'op:gc-answer))

(define-record-type <desc:import-object>
  (desc:import-object pos)
  desc:import-object?
  (pos desc:import-object-pos))
(define-values (marshall::desc:import-object unmarshall::desc:import-object)
  (make-marshallers <desc:import-object> #:name 'desc:import-object))

(define-record-type <desc:import-promise>
  (desc:import-promise pos)
  desc:import-promise?
  (pos desc:import-promise-pos))
(define-values (marshall::desc:import-promise unmarshall::desc:import-promise)
  (make-marshallers <desc:import-promise> #:name 'desc:import-promise))

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
(define-record-type <desc:export>
  (desc:export pos)
  desc:export?
  (pos desc:export-pos))
(define-values (marshall::desc:export unmarshall::desc:export)
  (make-marshallers <desc:export> #:name 'desc:export))

;; Something to answer that we haven't seen before.
;; As such, we need to set up both the promise import and this resolver/redirector
(define-record-type <desc:answer>
  (desc:answer pos)
  desc:answer?
  (pos desc:answer-pos))
(define-values (marshall::desc:answer unmarshall::desc:answer)
  (make-marshallers <desc:answer> #:name 'desc:answer))

;; This is a general sig-envelope, we might have some more specific
;; ones.  Whatever signed must refer to another serializable record
;; which is concretely typed.  If not, we run into confused deputy,
;; replay, oracle attack possibilities.  See also:
;;   https://sandstorm.io/news/2015-05-01-is-that-ascii-or-protobuf
;; Note that the key is not referred to; if it isn't obvious by the
;; payload and the protocol, then we aren't doing things right.
(define-record-type <desc:sig-envelope>
  (desc:sig-envelope signed signature)
  desc:sig-envelope?
  (signed desc:sig-envelope-signed)
  (signature desc:sig-envelope-signature))
(define-values (marshall::desc:sig-envelope unmarshall::desc:sig-envelope)
  (make-marshallers <desc:sig-envelope> #:name 'desc:sig-envelope))

;; Handoffs have three roles:
;;  - Gifter: who's sharing their import
;;  - Receiver: who's receiving the gift
;;  - Exporter: the location where the gift import is exported from
;;    (presumably, where it lives, though it may be a promise which
;;    eventually points to something else)

;; The handoff certificate from the gifter
(define-record-type <desc:handoff-give>
  (desc:handoff-give recipient-key exporter-location session gifter-side gift-id)
  desc:handoff-give?
   ;; handoff signing key this is being given to
   ;;   : handoff-key?
  (recipient-key desc:handoff-give-recipient-key)
   ;; exporter-location(-hint(s)): how to connect to get this
   ;;   : ocap-machine-uri?
   ;;   Note that currently this requires a certain amount of VatTP
   ;;   crossover, since we have to give a way to connect to VatTP...
  (exporter-location desc:handoff-give-exporter-location)
   ;; session: which session betweein gifter and exporter at the location
   ;;   : bytes?
  (session desc:handoff-give-session)
   ;; gifter-side: which "named side" of the session is the gifter
   ;;   : bytes?
  (gifter-side desc:handoff-give-gifter-side)
  ;; gift-id: The gift id associated with this gift
   ;;   : (or/c integer? bytes?)
  (gift-id desc:handoff-give-gift-id))

(define-values (marshall::desc:handoff-give unmarshall::desc:handoff-give)
  (make-marshallers <desc:handoff-give> #:name 'desc:handoff-give))

;; TODO: Maybe we only need the receiving-side, unsure
(define-record-type <desc:handoff-receive>
  (desc:handoff-receive receiving-session receiving-side handoff-count signed-give)
  desc:handoff-receive?
  (receiving-session desc:handoff-receive-receiving-session)
  (receiving-side desc:handoff-receive-receiving-side)
  (handoff-count desc:handoff-receive-handoff-count)
  (signed-give desc:handoff-receive-signed-give))

(define-values (marshall::desc:handoff-receive unmarshall::desc:handoff-receive)
  (make-marshallers <desc:handoff-receive> #:name 'desc:handoff-receive))

(define-record-type <op:start-session>
  (op:start-session captp-version handoff-pubkey acceptable-location acceptable-location-sig)
  op:start-session?
  (captp-version op:start-session-captp-version)
  (handoff-pubkey op:start-session-handoff-pubkey)
  ;; a sig-envelope signed by handoff-pubkey with a <my-location $location-data>
  (acceptable-location op:start-session-acceptable-location)
  (acceptable-location-sig op:start-session-acceptable-location-sig))

(define-values (marshall::op:start-session unmarshall::op:start-session)
  (make-marshallers <op:start-session> #:name 'op:start-session))

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
        marshall::op:start-session

        marshall::ocapn-machine
        marshall::ocapn-sturdyref))

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
        unmarshall::op:start-session

        unmarshall::ocapn-machine
        unmarshall::ocapn-sturdyref))

;; Doesn't verify that it's *valid*, just that it's *signed*
(define (signed-handoff-give? obj)
  (match obj
    [($ <desc:sig-envelope> (? desc:handoff-give? handoff-give-cert)
                            (? signature-sexp? sig))
     #t]
    [_ #f]))

(define (signed-handoff-receive? obj)
  (match obj
    [($ <desc:sig-envelope> ($ <desc:handoff-receive>
                               (? bytevector? session)
                               (? bytevector? session-side)
                               integer?
                               (? signed-handoff-give?))
                            (? signature-sexp? sig))
     #t]
    [_ #f]))


(define-record-type <internal-shutdown>
  (internal-shutdown type reason)
  internal-shutdown?
  (type internal-shutdown-type)
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
  (cmd-send-gc-export export-pos wire-delta)
  cmd-send-gc-export?
  (export-pos cmd-send-gc-export-export-pos)
  (wire-delta cms-send-gc-export-wire-delta))

;; We don't want to leak information about exceptions across CapTP boundries.
;; Eventually we want to have specific intentional error sharing across CapTP,
;; but until then we emit a mystery exception without additional information.
;; Leaking data is a security issue.
(define &mystery-exception (make-exception-type 'mystery &external-error '()))
(define make-mystery-exception (record-constructor &mystery-exception))

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
    (make-question-finder sealed-pos)
    question-finder?
    (sealed-pos question-finder-sealed-pos))

  (define (new-question-finder)
    (let ((question-finder (make-question-finder (pos-seal next-question-pos))))
      ;; Install our question at this question id.
      (hashq-set! questions question-finder next-question-pos)
      ;; Add it to the gc guardian.
      (guardian question-finder)
      ;; Increment the next-question id.
      (set! next-question-pos (add1 next-question-pos))
      question-finder))

  (define (_handle-message msg)
    (match msg
      [(or (? message?) (? questioned?))
       (<-np-extern internal-handler
                    (cmd-send-message msg))]
      [($ <listen-request> _ to-refr listener wants-partial?)
       (<-np-extern internal-handler
                    (cmd-send-listen to-refr listener
                                     wants-partial?))])
    _void)

  (define (_partition-unsealer-tm-cons)
    (cons partition-unseal partition-tm?))

  (define* (_listen-request to-refr listen-refr
                            #:key [wants-partial? #f])
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
        (let* ((noop-beh
                (lambda () 'no-op))
               (^cancel-sever-notification
                (lambda (bcom)
                  (lambda ()
                    ($C interested-in-sever 'remove sever-resolver)
                    (bcom noop-beh)))))
          ($C interested-in-sever 'add sever-resolver)
          (spawn ^cancel-sever-notification))]
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
    [new-question-finder new-question-finder]
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
  (define imports (make-weak-value-hash-table)) ; (eqv) imports:        chosen by peer
  (define questions (make-weak-key-hash-table)) ; (eq)  questions:      chosen by us
  (define answers (make-hash-table))            ; (eqv) answers:        chosen by peer

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
    (hash-for-each
     (lambda (import-pos count)
       ;; Send a gc-export message for this many
       (send-to-remote (op:gc-export import-pos count))
       ;; Reset these
       (hashv-remove! spare-import-counts import-pos))
     spare-import-counts))
  (define (decrement-exports-count-maybe-remove! export-pos delta)
    (assert-type export-pos integer?)
    (assert-type delta integer?)
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

  ;; A guardian to collect objects and question finders that are no
  ;; longer being referenced.
  (define guardian (make-guardian))

  (define (gc:question question-finder)
    (let ((question-pos (pos-unseal
                         (question-finder-sealed-pos question-finder))))
      (<-np-extern internal-handler (cmd-send-gc-answer question-pos))))

  (define (gc:import refr)
    (let* ((import-pos (pos-unseal (remote-refr-sealed-pos refr)))
           (spare-count (or (hashv-ref spare-import-counts import-pos) 0)))
      ;; We no longer need to keep track of the spare count.
      (hashv-remove! spare-import-counts import-pos)
      (<-np-extern internal-handler
                   ;; The number of references is one more than the number of
                   ;; spares.
                   (cmd-send-gc-export import-pos (+ spare-count 1)))))

  (define (captp-gc)
    (match (guardian)
      ;; Nothing in the guardian, so we're done.
      (#f #f)
      ;; Remote object or promise
      ((? remote-refr? refr)
       (gc:import refr)
       (captp-gc))
      ;; Question
      ((? question-finder? question-finder)
       (gc:question question-finder)
       (captp-gc))))

  ;; Spawn a fiber that periodically checks for garbage.
  (define (gc-loop)
    (sleep 1)
    (when running?
      (captp-gc)
      (gc-loop)))
  (spawn-fiber gc-loop)

  ;; Possibly install an export for this local refr, and return
  ;; this export id
  ;; TODO: we maybe need to differentiate between local-live-refr and
  ;;   remote-live-proxy-refr (once we set that up)?
  (define (maybe-install-export! refr)
    (assert-type refr live-refr?)
    (cond
     ;; Already have it, no need to increment next-export-pos
     [(hashq-ref exports-val2pos refr)
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

  (define (marshall-local-refr! local-refr)
    (assert-type local-refr local-refr?)
    (let ((export-pos (maybe-install-export! local-refr)))
      (match local-refr
        [(? local-object?)
         (desc:import-object export-pos)]
        [(? local-promise-refr?)
         (desc:import-promise export-pos)])))

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
      (hashv-set! imports import-pos new-refr)
      ;; add to the guardian...
      (guardian new-refr)
      ;; and return it.
      new-refr)
    (cond
     [(hashv-ref imports import-pos)
      =>
      (lambda (import)
        ;; Oh, we've already got that.  Reference and return it.
        (match import
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

  ;; general argument marshall/unmarshall for import/export

  ;; TODO: need to handle lists/dotted-lists/vectors
  (define (outgoing-pre-marshall! obj)
    (match obj
      [(obj ...)
       (map outgoing-pre-marshall! obj)]
      [(? hash-table?)
       ;; TODO: let's use "ghashes", which hash on eq? for live-refs
       ;; and on equal? for everything else
       (hash-fold
        (lambda (key val prev)
          (vhash-cons (outgoing-pre-marshall! key)
                      (outgoing-pre-marshall! val)
                      prev))
        vlist-null
        obj)]
      [(? set?)
       (set-fold
        (lambda (item this-set)
          (set-add this-set (outgoing-pre-marshall! item)))
        (make-set)
        obj)]
      [(? local-promise?)
       (desc:import-promise (maybe-install-export! obj))]
      [(? local-object?)
       (desc:import-object (maybe-install-export! obj))]
      [(? remote-refr?)
       (let ((refr-captp-connector (remote-refr-captp-connector obj)))
         (cond
          ;; from this captp
          [(eq? refr-captp-connector captp-connector)
           (desc:export (pos-unseal (remote-refr-sealed-pos obj)))]
          ;; elsewhere, let the coordinator do it
          [else
           ($C coordinator 'make-handoff-base-cert obj)]))]
      [(? void?)
       (make-syrec* 'void)]
      [(? keyword?)
       (make-syrec* 'kw (keyword->symbol obj))]
      [(? error?)
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
      [(? hash-table?)
       (hash-fold
        (lambda (key val prev)
          (vhash-cons (incoming-post-unmarshall! key)
                      (incoming-post-unmarshall! val)
                      prev))
        vlist-null
        obj)]
      [(? set?)
       (set-fold
        (lambda (item this-set)
          (set-add this-set (incoming-post-unmarshall! item)))
        (make-set)
        obj)]
      [(or (? desc:import-promise?) (? desc:import-object?))
       (maybe-install-import! obj)]
      [($ <desc:export> pos)
       (hashv-ref exports-pos2val pos)]
      [($ <syrec> 'exn:fail:mystery '())
       (make-exception
        (make-mystery-exception)
        (make-exception-with-message "Unknown error occured with remote object")
        (make-exception-with-irritants '()))]
      [($ <syrec> 'void '())
       _void]
      [($ <syrec> 'kw `(,keyword))
       (symbol->keyword keyword)]
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
       ($C coordinator 'start-retrieve-handoff sig-envelope-and-handoff)]
      [_ obj]))

  (define (unmarshall-to-desc to-desc)
    (match to-desc
      [($ <desc:export> export-pos)
       (hashv-ref exports-pos2val export-pos)]
      [($ <desc:answer> answer-pos)
       (hashv-ref answers answer-pos)]))

  (define (marshall-to obj)
    (match obj
      [(? question-finder?)
       (desc:answer (or (hashq-ref questions obj)
                        (error "No such entry in questions" obj)))]
      [(? remote-refr?)
       (let ((refr-captp-connector
              (remote-refr-captp-connector obj)))
         (cond
          ;; from this captp
          [(eq? refr-captp-connector captp-connector)
           (desc:export (pos-unseal (remote-refr-sealed-pos obj)))]
          [else
           (error 'captp-to-wrong-machine)]))]))

  (define (install-answer! answer-pos resolve-me-desc)
    (define resolve-me
      (maybe-install-import! resolve-me-desc))
    (when (hashv-ref answers answer-pos)
      (error 'already-have-answer
             "~a" answer-pos))
    (match-let (((answer-promise . answer-resolver)
                 (spawn-promise-cons)))
      (hashv-set! answers answer-pos answer-promise)
      (listen-to answer-promise resolve-me)
      (values answer-promise answer-resolver)))

  ;; Resolvers that are interested in when we're tearing down
  (define interested-in-sever
    (spawn ^seteq))

  (define (tear-it-down shutdown-type reason)
    (set! exports-val2pos #f)
    (set! exports-pos2val #f)
    (set! imports #f)
    (set! questions #f)
    (set! answers #f)
    (set! running? #f)
    (for-each
     (lambda (interested)
       (<-np interested 'fulfill (list 'severed shutdown-type
                                       reason)))
     ($C interested-in-sever 'as-list))
    (set! interested-in-sever #f))

  ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ;; TODO TODO TODO: EACH of these needs to call (handle-spare-imports!)
  ;; at the end of its behavior!  Probably the best thing to do is to
  ;; make a wrapper for each of these that does so... and also which
  ;; adds an error handler which aborts the whole thing if such an
  ;; error occurs
  ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  (define (^captp-incoming-handler bcom)
    (lambda (msg)
      (unless running?
        (error 'captp-breakage "Captp session is no longer running but got ~a"
               msg))
      (match msg
        [($ <op:bootstrap> (? integer? answer-pos) resolve-me-desc)
         (let-values (((_answer-promise answer-resolver)
                       (install-answer! answer-pos resolve-me-desc)))
           ;; And since we're bootstrapping, we resolve it immediately
           ($C answer-resolver 'fulfill bootstrap-obj)
           _void)]
        ;; TODO: Handle case where the target doesn't exist?
        ;;   Or maybe just generally handle unmarshalling errors :P
        [($ <op:deliver-only> to-desc args-marshalled)
         (let*-values (((args)
                        (incoming-post-unmarshall! args-marshalled))
                       ((target) (unmarshall-to-desc to-desc)))
           (apply <-np target args)
           _void)]
        [($ <op:deliver> to-desc
                         args-marshalled
                         ;; answer-pos is either an integer (promise pipelining)
                         ;; or #f (no pipelining)
                         (and (or (? integer?) #f)
                              answer-pos)
                         resolve-me-desc)
         (define (do-it)
           (define args
             (incoming-post-unmarshall! args-marshalled))
           (define target (unmarshall-to-desc to-desc))
           (define sent-promise
             (apply <- target args))
           ;; We're either resolving the to the answer promise we create
           ;; or we're resolving to the actual object described by resolve-me-desc
           ;;
           ;; The former case is an indirection because messages pipelined
           ;; to a to-be-answered object are simply sent to a local promise
           ;; which will eventually resolve to the answer.
           ;;
           ;; It's possible that something more efficient could be done
           ;; than throwing in an intermediate promise pair that we inform the
           ;; other side of; maybe re-evaluate when we handle
           ;; automatic-severance-on-session-disconnect.
           (cond
            [answer-pos
             (let-values (((_answer-promise answer-resolver)
                           (install-answer! answer-pos resolve-me-desc)))
               ($C answer-resolver 'fulfill sent-promise))]
            [else
             (let ((to-resolve
                    (maybe-install-import! resolve-me-desc)))
               (<-np to-resolve 'fulfill sent-promise))])
           _void)
         (do-it)]

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
        [($ <op:listen> (? desc:export? to-desc)
                        (? desc:import? listener-desc)
                        (? boolean? wants-partial?))
         (let ((to-refr
                (unmarshall-to-desc to-desc))
               (listener
                (incoming-post-unmarshall! listener-desc)))
           (listen-to to-refr listener
                      #:wants-partial? wants-partial?)
           _void)]
        [($ <op:gc-answer> answer-pos)
         (hashv-remove! answers answer-pos)]
        [($ <op:gc-export> (? integer? export-pos) (? integer? wire-delta))
         (decrement-exports-count-maybe-remove! export-pos wire-delta)]
        [($ <op:abort> (? string? reason))
         (tear-it-down 'abort reason)]
        [($ <internal-shutdown> (? symbol? type) (? string? reason))
         (when (eq? type 'abort)
           (send-to-remote (op:abort reason)))
         (tear-it-down type reason)]
        [other-message
         (error 'invalid-message "~a" other-message)])))

  (define (^internal-handler bcom)
    (lambda (cmd)
      (define (running-handle-cmd cmd)
        (match cmd
          [($ <cmd-send-message> msg)
           (define-values (real-msg answer-pos)
             (match msg
               [(? message?)
                (values msg #f)]
               [($ <questioned> msg answer-this-question)
                (values msg (hashq-ref questions answer-this-question))]))
           (match-let ((($ <message> _ to resolve-me args)
                        real-msg))
             (define deliver-msg
               (if resolve-me
                   (op:deliver (marshall-to to)
                               (outgoing-pre-marshall! args)
                               answer-pos
                               (marshall-local-refr! resolve-me))
                   (op:deliver-only (marshall-to to)
                                    (outgoing-pre-marshall! args))))
             (send-to-remote deliver-msg))]
          [($ <cmd-send-listen> (? remote-refr? to-refr) (? local-refr? listener-refr)
                                (? boolean? wants-partial?))
           (let ((listen-msg
                  (op:listen (marshall-to to-refr)
                             (outgoing-pre-marshall! listener-refr)
                             wants-partial?)))
             (send-to-remote listen-msg))]
          [($ <cmd-send-gc-answer> (? integer? answer-pos))
           (send-to-remote (op:gc-answer answer-pos))]
          [($ <cmd-send-gc-export> (? integer? export-pos) (? integer? wire-delta))
           (send-to-remote (op:gc-export export-pos wire-delta))]))
      (define (broken-handle-cmd cmd)
        (match cmd
          [($ <cmd-send-message> msg)
           (define resolve-me
             (match msg
               ((? message?)
                (message-resolve-me msg))
               ))
           (match-let ((($ <message> _ to resolve-me args)
                        msg))
             (when resolve-me
               (<-np resolve-me 'break (captp-session-severed))))]
          [($ <cmd-send-listen> (? remote-refr? to-refr) (? local-refr? listener-refr)
                                (? boolean? wants-partial?))
           (<-np listener-refr 'break (captp-session-severed))]
          [($ <cmd-send-gc-answer> (? integer? answer-pos))
           'no-op]
          [($ <cmd-send-gc-export> (? integer? export-pos))
           'no-op]))
      (if running?
          (running-handle-cmd cmd)
          (broken-handle-cmd cmd))))

  (define captp-incoming-handler
    (spawn ^captp-incoming-handler))
  (define internal-handler
    (spawn ^internal-handler))

  ;; BEGIN REMOTE BOOTSTRAP OPERATION
  ;; ================================
  (define this-question-finder
    (new-question-finder))
  (define-values (remote-bootstrap-vow remote-bootstrap-resolver)
    (_spawn-promise-values #:question-finder
                           this-question-finder
                           #:captp-connector
                           captp-connector))
  (define bootstrap-msg
    (op:bootstrap (hashq-ref questions this-question-finder)
                  (outgoing-pre-marshall! remote-bootstrap-resolver)))
  (send-to-remote bootstrap-msg)
  ;; END REMOTE BOOTSTRAP OPERATION
  ;; ==============================
  (values captp-incoming-handler remote-bootstrap-vow))

(define* (^coordinator bcom router our-location
                       intra-machine-warden intra-machine-incanter
                       #:key [handoff-key-pair (generate-key-pair)]
                       ;; #:local-machine-location [local-machine-location #f]
                       )
  ;; counters used to increment how many handoff requests have been
  ;; made in this session to prevent replay attacks.
  ;; every time a *request* is made, this should be incremented.
  (define our-handoff-count 0)
  ;; TODO TODO TODO: We need to make use of this and also check the
  ;;   session listed on the receive certificate to prevent a replay
  ;;   attack
  (define remote-handoff-count 0)

  ;; (define handoff-key-pair
  ;;   (generate-key
  ;;    (sexp->canonical-sexp
  ;;     '(genkey (eddsa (curve Ed25519) (flags eddsa))))))

  (define handoff-privkey
    (key-pair->private-key handoff-key-pair))

  (define handoff-pubkey
    (key-pair->public-key handoff-key-pair))

  (define (get-handoff-pubkey)
    (gcrypt:pk-crypto:canonical-sexp->sexp handoff-pubkey))

  ;; TODO: maybe the hashing isn't necessary
  (define our-side-name
    (sha256d (syrup-encode (get-handoff-pubkey))))

  (define our-location-sig
    (let ((encoded-location
           (syrup-encode
            (make-syrec* 'my-location our-location)
            #:marshallers marshallers)))
      (sign encoded-location handoff-privkey)))

  (define core-beh
    (methods
     [(get-suite) 'prot0]
     [(get-our-side-name) our-side-name]
     [get-handoff-pubkey get-handoff-pubkey]
     ;; TODO: Horrible, we need to protect against this
     [(get-handoff-privkey) handoff-privkey]
     [(get-location-sig) our-location-sig]))

  (define pre-init-beh
    (extend-methods core-beh
      [(install-remote-key remote-encoded-key
                           remote-handoff-key
                           remote-location)
       (bcom (ready-beh remote-encoded-key
                        remote-handoff-key
                        remote-location)
             'OK)]))

  (define (ready-beh remote-encoded-key
                     remote-key
                     remote-location
                     ;; remote-machine-location     ;; auughhhhhh
                     )
    (define remote-side-name
      (sha256d (syrup-encode remote-encoded-key)))
    (when (equal? remote-side-name our-side-name)
      (error "Both sides can't share the same name / signing key!"))

    ;; Both sides should converge on the same session name if all goes well
    ;; because both sides should have sorted by bytes
    (define session-name
      (sha256d (apply bytes-append
                      (bytes "prot0")
                      (sort (list remote-side-name our-side-name)
                            bytes<?))))

    ;; NOTE: Every session requires that both ends generate brand
    ;; new keypairs.
    ;; Thus we could probably have a unique derived key per session
    ;; directly from the secret derivation with no additional step?
    ;; But I'm unsure about this.  It may be good hygiene if we
    ;; use a shared keypair to derive a CEK (Content Encryption Key)
    ;; anyway...
    #;(define shared-secret ...)

    (define (make-handoff-base-cert exported-remote-refr)
      ;; TODO: Bail out early if we've already disconnected
      (define exported-captp-connector
        (remote-refr-captp-connector exported-remote-refr))
      (define exported-connector-obj
        (exported-captp-connector 'connector-obj))
      (define recipient-key remote-encoded-key)
      (define exporter-location
        ($C intra-machine-incanter
            exported-connector-obj 'get-remote-location))
      (define gifter-and-exporter-session
        ($C intra-machine-incanter exported-connector-obj
            'get-session-name))
      (define gifter-side
        ($C intra-machine-incanter exported-connector-obj
            'get-our-side-name))
      (define gift-id (strong-random-bytes 32))

      (define handoff-give
        (desc:handoff-give recipient-key
                           exporter-location gifter-and-exporter-session
                           gifter-side
                           gift-id))
      (define handoff-give-sig
        (sign (syrup-encode handoff-give
                            #:marshallers marshallers)
              ($C intra-machine-incanter
                  exported-connector-obj 'get-handoff-privkey)))

      (define exporter-session-bootstrap
        ($C intra-machine-incanter
            exported-connector-obj 'get-remote-bootstrap))

      (unless (exported-captp-connector 'same-connection? exported-remote-refr)
        (error "Tried to deposit a gift not at the remote location"))

      ;; Now we send a message to the exporter saying we'd like to deposit
      ;; this gift
      (<-np exporter-session-bootstrap 'deposit-gift
            gift-id exported-remote-refr)

      (desc:sig-envelope handoff-give handoff-give-sig))

    (define (start-retrieve-handoff signed-handoff-give)
      (assert-type signed-handoff-give signed-handoff-give?)
      (let ((exporter-location
             (desc:handoff-give-exporter-location
              (desc:sig-envelope-signed signed-handoff-give))))
        (cond
         ;; Oh, this is us.  Well, we don't need to open a new session
         ;; for that, though we do need to coordinate with whatever
         ;; session is in question
         [($C router 'self-location? exporter-location)
          ;; In order for this to happen, we have to be getting a
          ;; handoff with ourselves as the gifter!  Yikes!  Well,
          ;; this can happen accidentally if A and B have two simultaneous
          ;; sessions open with each other.
          ;;
          ;; Note that this might be caused by the crossed hellos problem
          ;; (or simply that even from the outgoing connection side, we
          ;; don't bother to deduplicate while attempting a connection...
          ;; oops)
          ;;
          ;; TODO: Fix crossed hellos problem
          ;; TODO: Fix simultaneous outgoing connections problem, which
          ;;   is related, but easier to fix.  To do so we just need to
          ;;   recognize that we're "in the middle of" establishing a
          ;;   connection and buffer multiple waiting connection attempts
          ;;   together.  It would be okay for them to all fail together
          ;;   if something goes wrong.  We can delay thinking about whether
          ;;   or not to supply a re-connect until later.
          (error "Handoff points at ourselves... crossed hellos or adjacent problem?")]
         ;; Oh, this is someone else.
         ;; Well, we're going to need to make a receive certificate
         ;; and work with the router to pass it along
         [else
          (let* ((handoff-receive
                  (desc:handoff-receive session-name our-side-name
                                        our-handoff-count signed-handoff-give))
                 (handoff-receive-sig
                  (sign (syrup-encode handoff-receive
                                      #:marshallers marshallers)
                        handoff-privkey))
                 (signed-handoff-receive
                  (desc:sig-envelope handoff-receive
                                     handoff-receive-sig)))
            ;; maybe a cell would be better, dunno
            (set! our-handoff-count (add1 our-handoff-count))
            (<- router 'send-handoff-receive signed-handoff-receive))])))

    (define (give-handoff-legit? signed-handoff-give)
      (assert-type signed-handoff-give signed-handoff-give?)
      (match-let* ((($ <desc:sig-envelope>
                       (? desc:handoff-give? handoff-give)
                       (? signature-sexp? give-sig-sexp))
                    signed-handoff-give)
                   (($ <desc:handoff-give>
                       _give-recipient-encoded-key
                       give-exporter-location
                       give-session
                       give-gifter-side
                       _give-gift-id)
                    handoff-give)
                   (encoded-handoff-give
                    (syrup-encode handoff-give
                                  #:marshallers marshallers))
                   (give-sig
                    (gcrypt:pk-crypto:sexp->canonical-sexp
                     give-sig-sexp)))
        (and (equal? session-name give-session)
             (equal? give-gifter-side remote-side-name)
             ;; I'm not sure if this one is critical.
             ;; Should consider the attack scenarios again.
             ;; Probably doesn't hurt; maybe can just leave it until
             ;; we find a reason not to.
             ($C router 'self-location? give-exporter-location)
             (verify give-sig encoded-handoff-give remote-key))))

    (define (full-handoff-legit? signed-handoff-receive)
      (assert-type signed-handoff-receive signed-handoff-receive?)
      (match-let* ((($ <desc:sig-envelope> (and handoff-receive
                                            ($ <desc:handoff-receive>
                                             ;; TODO: verify these three where appropriate
                                             ;; (probably not in this session, which is
                                             ;; with the gifter, but with the receiver)
                                             (? bytevector? _handoff-session)
                                             (? bytevector? _handoff-session-side)
                                             (? integer? _this-handoff-count)
                                             signed-handoff-give))
                                       (? signature-sexp? receive-sig-sexp))
                    signed-handoff-receive)
                   (encoded-handoff-receive
                    (syrup-encode handoff-receive
                                  #:marshallers marshallers))
                   (give-recipient-encoded-key
                    (desc:handoff-give-recipient-key
                     (desc:sig-envelope-signed signed-handoff-give)))
                   (give-recipient-key
                    (gcrypt:pk-crypto:sexp->canonical-sexp
                     give-recipient-encoded-key))
                   (receive-sig
                    (gcrypt:pk-crypto:sexp->canonical-sexp
                     receive-sig-sexp)))
        (and (give-handoff-legit? signed-handoff-give)
             (verify receive-sig encoded-handoff-receive give-recipient-key))))

    (extend-methods core-beh
      [(get-remote-side-name) remote-side-name]
      [(get-remote-location) remote-location]
      [(get-session-name) session-name]
      [(get-our-side-name) our-side-name]
      ;; handoff stuff
      [make-handoff-base-cert make-handoff-base-cert]
      [start-retrieve-handoff start-retrieve-handoff]
      [full-handoff-legit? full-handoff-legit?]
      [give-handoff-legit? give-handoff-legit?]))

  pre-init-beh)

(define-record-type <giftmeta>
  (make-giftmeta gift destroy-on-fetch)
  giftmeta?
  (gift giftmeta-gift)
  (destroy-on-fetch giftmeta-destroy-on-fetch))

(define-record-type <sessionmeta>
  (make-sessionmeta location
                    local-bootstrap-obj remote-bootstrap-obj
                    coordinator session-name)
  sessionmeta?
  (location sessionmeta-location)
  (local-bootstrap-obj sessionmeta-local-bootstrap-obj)
  (remote-bootstrap-obj sessionmeta-remote-bootstrap-obj)
  (coordinator sessionmeta-coordinator)
  (session-name sessionmeta-session-name))

(define* (spawn-mycapn #:key [custom-bootstrap #f]
                       #:rest netlayers)
  (define netlayer-map
    (spawn ^ghash
           (fold
            (lambda (netlayer netmap)
              (ghash-set netmap ($C netlayer 'netlayer-name)
                         netlayer))
            ghash-null
            netlayers)))

  (define (^mycapn bcom)
    ;; For sturdyrefs
    ;; TODO: Eventually... well this whole sturdyref nonsense we want
    ;; to make more configureable
    (define-values (registry locator)
      (spawn-nonce-registry-and-locator))

    ;; Warden and incanter for collaborating parties in this
    ;; particular machine
    (define-values (intra-machine-warden intra-machine-incanter)
      (spawn-warding-pair))
    (define locations->open-session-names
      (spawn ^ghash))
    (define open-session-names->sessionmeta
      (spawn ^ghash))

    ;; For keeping track of new outbound sessions to detect crossed hellos
    (define locations->crossed-hellos-resolver
      (spawn ^ghash))

    (define (^connection-establisher bcom netlayer netlayer-name)
      (lambda (read-message write-message remote-connect-location)
        ($C self 'new-connection netlayer netlayer-name
            read-message write-message remote-connect-location)))

    (define* (^bootstrap bcom coordinator #:key [extends #f])
      (define session-name ($C coordinator 'get-session-name))
      (define gifts
        (spawn ^ghash))
      (define waiting-gifts
        (spawn ^ghash))

      (define (valid-gift-id? x)
        (or (integer? x)
            (bytevector? x)
            (string? x)))
      ;; In the case of depositing, we're putting a gift at gift-id and an
      ;;
      (define (deposit-gift gift-id obj
                            ;; #:manual-drop? [manual-drop? #f]
                            )
        (assert-type gift-id valid-gift-id?)
        (assert-type obj local-refr?)
        (when ($C waiting-gifts 'has-key? gift-id)
          (match ($C waiting-gifts 'ref gift-id)
            [(_gift-promise gift-resolver)
             ($C gift-resolver 'fulfill obj)
             ($C waiting-gifts 'remove gift-id)]))
        ($C gifts 'set gift-id (make-giftmeta obj #t)))

      (define (withdraw-gift signed-handoff-receive)
        (assert-type signed-handoff-receive signed-handoff-receive?)
        (match-let* ((handoff-receive
                      (desc:sig-envelope-signed signed-handoff-receive))
                     (handoff-give
                      (desc:sig-envelope-signed
                       (desc:handoff-receive-signed-give handoff-receive)))
                     (session-id
                      (desc:handoff-give-session handoff-give))
                     (($ <sessionmeta> cert-session-location
                                       cert-session-local-bootstrap-obj
                                       cert-session-remote-bootstrap-obj
                                       cert-session-coordinator
                                       cert-session-session-name)
                      (if ($C open-session-names->sessionmeta 'has-key? session-id)
                          ($C open-session-names->sessionmeta 'ref session-id)
                          (begin
                            (error 'no-open-session "No open session with key ~s"
                                   session-id)))))
          ;; TODO: count stuff here too, but needs to be in this session
          (unless ($C cert-session-coordinator 'full-handoff-legit? signed-handoff-receive)
            (error 'invalid-handoff-cert
                   "Handoff cert invalid for session: ~s"
                   signed-handoff-receive))

          ;; If we made it this far, it's ok... so time to get that referenced
          ;; object!
          ($C intra-machine-incanter cert-session-local-bootstrap-obj
              'pull-out-gift
              (desc:handoff-give-gift-id handoff-give))))

      (define main-beh
        (extend-methods extends
          [deposit-gift deposit-gift]
          [withdraw-gift withdraw-gift]
          [(fetch swiss-num)
           ($C locator 'fetch swiss-num)]))

      (define cross-gift-beh
        (methods
         [(pull-out-gift id)
          (cond
           [($C gifts 'has-key? id)
            (match-let ((($ <giftmeta> gift destroy-on-fetch?)
                         ($C gifts 'ref id)))
              (when destroy-on-fetch?
                ($C gifts 'remove id))
              gift)]
           ;; queue it
           [else
            (if ($C waiting-gifts 'has-key? id)
                (match ($C waiting-gifts 'ref id)
                  [(gift-promise _gift-resolver)
                   gift-promise])
                (let-values ([(gift-promise gift-resolver)
                              (spawn-promise-values)])
                  ($C waiting-gifts 'set id (list gift-promise gift-resolver))
                  gift-promise))])]))

      (ward intra-machine-warden cross-gift-beh #:extends main-beh))

    #;(define (^bootstrap bcom coordinator #:extends [extends #f])
    (define session-name ($C coordinator 'get-session-name))
    (methods
    #:extends extends
    [(deposit-gift gift-id obj)
    (pk 'deposit-gift gift-id obj)
    'TODO]
    [(withdraw-gift signed-handoff-receive)
    (pk 'retrieve-gift signed-handoff-receive)
    'TODO]))

    ;; TODO: Rename this to connect-to-machine I guess?
    (define (retrieve-or-setup-session-vow remote-machine-loc)
      (if ($C locations->open-session-names 'has-key? remote-machine-loc)
          ;; found an open session for this location
          (let ([session-name ($C locations->open-session-names
                                  'ref remote-machine-loc)])
            (sessionmeta-remote-bootstrap-obj
             ($C open-session-names->sessionmeta 'ref session-name)))
          ;; Guess we'll make a new one
          (let ([netlayer (get-netlayer-for-location remote-machine-loc)])
            ($C netlayer 'connect-to remote-machine-loc))))

    (define (get-netlayer-for-location loc)
      (define transport-tag (ocapn-machine-transport loc))
      (unless ($C netlayer-map 'has-key? transport-tag)
        (error 'unsupported-transport
               "NETLAYER not supported for this machine: ~a" transport-tag))
      ($C netlayer-map 'ref transport-tag))

    (define (self-location? loc)
      (define netlayer (get-netlayer-for-location loc))
      ($C netlayer 'self-location? loc))

    ;; Sturdyref stuff, to be refactored
    ;; TODO: expiry/revocation/unregistry
    (define (register obj netlayer-name)
      (assert-type obj live-refr?)
      (assert-type netlayer-name symbol?)
      (unless ($C netlayer-map 'has-key? netlayer-name)
        (error 'unsupported-transport
               "NETLAYER not supported for this machine: ~a" netlayer-name))
      (let* ((netlayer ($C netlayer-map 'ref netlayer-name))
             (machine-loc ($C netlayer 'our-location))
             (nonce ($C registry 'register obj)))
        (make-ocapn-sturdyref machine-loc nonce)))
    (define (enliven sturdyref)
      (assert-type sturdyref ocapn-sturdyref?)
      (let ((sref-loc (ocapn-sturdyref-machine sturdyref))
            (sref-swiss-num (ocapn-sturdyref-swiss-num sturdyref)))
        ;; Is it local?
        (if (self-location? sref-loc)
            ($C locator 'fetch sref-swiss-num)
            (<- (retrieve-or-setup-session-vow sref-loc) 'fetch
                sref-swiss-num))))

    ;; Here's why all netlayers currently are in the same vat as the
    ;; router, at least currently... to make sure that they're all set up
    ;; before we continue further.
    ;;
    ;; This could be done via some promise-chaining stuff but it's probably
    ;; best as-is.
    (ghash-for-each
     (lambda (netlayer-name netlayer)
       ($C netlayer 'setup (spawn ^connection-establisher netlayer netlayer-name)))
     ($C netlayer-map 'data))

    (methods
     [(send-handoff-receive signed-handoff-receive)
      (define handoff-give
        (desc:sig-envelope-signed
         (desc:handoff-receive-signed-give
          (desc:sig-envelope-signed
           signed-handoff-receive))))
      (define exporter-location
        (desc:handoff-give-exporter-location handoff-give))
      ;; "Returning home" should be handled in start-retrieve-handoff
      (when (self-location? exporter-location)
        (error "self-handoff-receive called with self-location"))
      (define session-bootstrap-vow
        (retrieve-or-setup-session-vow exporter-location))
      (<- session-bootstrap-vow 'withdraw-gift signed-handoff-receive)]

     ;; TODO: we should also allow some way to shut things down here or
     ;; somewhere...
     ;; TODO: Should this still be an exposed method?  Maybe it's something only
     ;; the ^connection-establisher should call...
     [(new-connection netlayer netlayer-name read-message write-message remote-connect-location)
      (define-values (captp-outgoing-enq-ch captp-outgoing-deq-ch captp-outgoing-stop?)
        (spawn-delivery-agent))
      (define (send-to-remote msg)
        (put-message captp-outgoing-enq-ch msg)
        _void)
      (define our-location
        ($C netlayer 'our-location))
      (define coordinator
        (spawn ^coordinator self our-location
               intra-machine-warden intra-machine-incanter))
      (define handoff-pubkey
        ($C coordinator 'get-handoff-pubkey))
      (define our-location-sig
        ($C coordinator 'get-location-sig))

      ;; We don't actually have a bootstrap vow until setup completion, so
      ;; we'll have to return a vow to a vow
      (define-values (meta-bootstrap-vow meta-bootstrap-resolver)
        (spawn-promise-values))

      ;; Complete the initialization step against the remote machine.
      ;; Basically this allows the coordinator to know of what remote
      ;; key will be used in this session.
      (define (^setup-completer bcom)
        (match-lambda
          ;; TODO: Shouldn't the netlayer actually interpret this message
          ;;   before it gets here?  Ie, at this stage, we're already
          ;;   "confident" this is from the right location
          [($ <op:start-session>
              (? string? remote-captp-version)
              remote-encoded-pubkey
              ;; TODO: We want to restores something like the below, which
              ;;   is what the racket version expects, or at least unify the
              ;;   two.
              #;(and remote-encoded-pubkey
                 ('eddsa 'public 'ed25519 _))
              (? ocapn-machine? claimed-remote-location)
              encoded-remote-location-sig)

           ;; Check we are speaking the same language!
           (unless (string=? remote-captp-version captp-version)
             ;; Needs to be <-np-extern so that the error that is
             ;; thrown after doesn't cancel dispatch.
             (<-np-extern incoming-forwarder
                          (internal-shutdown 'abort "CapTP version is incompatible"))
             (error (format #f "CapTP version is incompatible (our version: ~a, remote version: ~a)"
                            captp-version
                            remote-captp-version)))

           (define remote-handoff-pubkey
             (gcrypt:pk-crypto:sexp->canonical-sexp remote-encoded-pubkey))
           ;; TODO: I guess we didn't know by the time this was opened
           ;;   what the remote location was going to be... that's part of the reason
           ;;   for the start-session message...
           ;;   So, remove this if we can.  Or realistically, move this whole part
           ;;   to the NETLAYER code.
           #;(unless (same-machine-location? claimed-remote-location remote-location)
           (error (format "Supplied location mismatch. Claimed: ~s Expected: ~s"
           claimed-remote-location remote-location)))

           (define encoded-location
             (syrup-encode
              (make-syrec* 'my-location claimed-remote-location)
              #:marshallers marshallers))
           (define remote-location-sig
             (gcrypt:pk-crypto:sexp->canonical-sexp encoded-remote-location-sig))

           (unless (verify remote-location-sig encoded-location remote-handoff-pubkey)
             (let ((reason "Invalid location signature"))
               ;; Needs to be <-np-extern so that the error that is
               ;; thrown after doesn't cancel dispatch.
               (<-np-extern incoming-forwarder
                            (internal-shutdown 'abort reason))
               (error 'captp-invalid-signature remote-location-sig)))

           ;; TODO: Now we need to do the dial back and verify that
           ;; the location is where it says it is!

           ;; Now that we've verified:

           (define remote-location claimed-remote-location)

           ;; Now use it to finish initializing the coordinator
           ($C coordinator 'install-remote-key
               remote-encoded-pubkey
               remote-handoff-pubkey
               remote-location)

           ;; Mitigation here.
           (define can-continue?
             (let* ((chr ($C locations->crossed-hellos-resolver 'ref remote-location #f))
                    (their-side-name ($C coordinator 'get-remote-side-name))
                    (outgoing? (ocapn-machine? remote-connect-location))
                    (must-abort? (if (and chr (not outgoing?)) ($C chr their-side-name) #f)))
               ;; Clean up the crossed hellos resolver actor, we won't need it after this.
               (unless (null? chr)
                 ($C locations->crossed-hellos-resolver 'remove remote-location))
               ;; Send internal shutdown if needed.
               (when must-abort?
                 (<- incoming-forwarder (internal-shutdown 'abort "Crossed hellos mitigation")))
               (not must-abort?)))

           (define (make-local-bootstrap-obj)
             (if custom-bootstrap
                 (custom-bootstrap ^bootstrap coordinator)
                 (spawn ^bootstrap coordinator)))

           (when can-continue?
             (let*-values (((session-name) ($C coordinator 'get-session-name))
                           ((local-bootstrap-obj) (make-local-bootstrap-obj))
                           ((captp-incoming-handler remote-bootstrap-vow)
                            (setup-captp-conn send-to-remote coordinator
                                              local-bootstrap-obj
                                              intra-machine-warden intra-machine-incanter)))
               ;; Fulfill the meta-bootstrap-promise with the promise that
               ;; setup-captp-conn gave us
               ($C meta-bootstrap-resolver 'fulfill remote-bootstrap-vow)
               ;; And set things up so that the incoming-forwarder now goes
               ;; to the captp-incoming-handler
               (incoming-swap captp-incoming-handler)

               ;; TODO: Deal with duplicate sessions and also "crossed connections"

               ;; And now install in the open sessions in the directory
               ($C locations->open-session-names 'set remote-location session-name)
               ($C open-session-names->sessionmeta 'set
                   session-name
                   (make-sessionmeta remote-location
                                     local-bootstrap-obj remote-bootstrap-vow
                                     coordinator session-name))))
           _void]
          ;; Handle shutdown requests that happen before the setup
          ;; completer hands control to the internal handler.
          [($ <internal-shutdown> (? symbol? type) (? string? reason))
           (when (eq? type 'abort)
             (send-to-remote (op:abort reason)))
           ;; Since we're shutting down, our new behavior will be to
           ;; ignore all further messages.
           (bcom (lambda _ _void))]))

      (define-values (incoming-forwarder incoming-swap)
        (swappable (spawn ^setup-completer)))

      ;; Now spawn fibers that read/write to these ports
      (syscaller-free-fiber
       (lambda ()
         (let lp ()
           (match (read-message unmarshallers)
             [(? eof-object?)
              (<-np-extern incoming-forwarder
                           (internal-shutdown 'disconnect "Remote disconnected"))]
             [msg
              (<-np-extern incoming-forwarder msg)
              (lp)]))))

      (syscaller-free-fiber
       (lambda ()
         (let lp ()
           (define msg
             (get-message captp-outgoing-deq-ch))
           (write-message msg marshallers)
           ;; (syrup-write msg network-out-port #:marshallers marshallers)
           ;; ;; TODO: *should* we be flushing output each time we've written out
           ;; ;; a message?  It seems like "yes" but I'm a bit unsure
           ;; (force-output network-out-port)
           (lp))))

      ;; The crossed hellos problem is where we try to connect to a location
      ;; at the same time, they are trying to connect to us. Only one of these
      ;; connections should be allowed to succeed.
      ;;
      ;; In CapTP when each side opens a connection it MUST send its op:start-session
      ;; message first. When each side has received this, it then should look and
      ;; detect crossed hellos (we look in the locations->crossed-hellos-resolver table).
      ;;
      ;; If we detect the crossed hellos problem we take our key from our outbound
      ;; connection and compute the side name (our-side-name) and the remote key
      ;; from the inbound connection and calculate their name (their-side-name).
      ;; With both of these, we sort them bytewise and whichever is lower, that
      ;; session ends, the higher of the two continues.
      (define (^crossed-hellos-resolver bcom our-side-name)
        (define voided-beh (lambda _ _void))
        (lambda (remote-side-name)
          (let ([sorted-names (sort (list our-side-name remote-side-name) bytes<?)])
            (if (equal? (car sorted-names) our-side-name)
                (begin
                  (<- incoming-forwarder (internal-shutdown 'abort "Crossed hellos mitigation"))
                  (bcom voided-beh #f))
                (bcom voided-beh #t)))))

      (when (ocapn-machine? remote-connect-location)
        ($C locations->crossed-hellos-resolver 'set
           remote-connect-location
           (spawn ^crossed-hellos-resolver ($C coordinator 'get-our-side-name))))

      ;; Send our op:start-session message to the other side, which will be
      ;; handled by the ^setup-completer above.
      (send-to-remote (op:start-session captp-version
                                        handoff-pubkey
                                        our-location
                                        our-location-sig))

      ;; Return the meta-bootstrap-vow, which will be completed as above
      meta-bootstrap-vow]

     [self-location? self-location?]
     ;; ... is that it?
     [connect-to-machine retrieve-or-setup-session-vow]

     [(install-netlayer netlayer)
      (define netlayer-name ($C netlayer 'netlayer-name))
      (when ($C netlayer-map 'hash-has-key? netlayer-name)
        (error (format #f "Already has netlayer key ~a" netlayer-name)))
      ($C netlayer-map 'set netlayer-name netlayer)]
     [register register]
     [enliven enliven]
     ;; Get the nonce registry used for sturdyrefs to be able to tweak
     ;; it directly.
     [(get-registry) registry]))
  (define self (spawn ^mycapn))
  self)
