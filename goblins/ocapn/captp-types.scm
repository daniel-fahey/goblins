;;; Copyright 2019-2021 Christine Lemmer-Webber
;;; Copyright 2023 David Thompson
;;; Copyright 2024 Jessica Tallon
;;; Copyright 2024 Juliana Sims
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

;; This module is private and should only be used by captp and gc.
(define-module (goblins ocapn captp-types)
  #:use-module (goblins contrib syrup)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins utils crypto)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (<captp-session-severed>
            captp-session-severed
            captp-session-severed?

            <op:deliver-only>
            op:deliver-only
            op:deliver-only?
            op:deliver-only-to-desc
            op:deliver-only-args

            <op:deliver>
            op:deliver
            op:deliver?
            op:deliver-to-desc
            op:deliver-args
            op:deliver-answer-pos
            op:deliver-resolve-me-desc

            <op:abort>
            op:abort
            op:abort?
            op:abort-reason

            <op:listen>
            op:listen
            op:listen?
            op:listen-to-desc
            op:listen-listener-desc
            op:listen-wants-partial?

            <op:gc-export>
            op:gc-export
            op:gc-export?
            op:gc-export-export-pos
            op:gc-export-wire-delta

            <op:gc-answer>
            op:gc-answer
            op:gc-answer?
            op:gc-answer-answer-pos

            <desc:import-object>
            desc:import-object
            desc:import-object?
            desc:import-object-pos

            <desc:import-promise>
            desc:import-promise
            desc:import-promise?
            desc:import-promise-pos

            desc:import-pos
            desc:import?

            <desc:export>
            desc:export
            desc:export?
            desc:export-pos

            <desc:answer>
            desc:answer
            desc:answer?
            desc:answer-pos

            <desc:sig-envelope>
            desc:sig-envelope
            desc:sig-envelope?
            desc:sig-envelope-signed
            desc:sig-envelope-signature

            <desc:handoff-give>
            desc:handoff-give
            desc:handoff-give?
            desc:handoff-give-receiver-key
            desc:handoff-give-exporter-location
            desc:handoff-give-gifter-exporter-session
            desc:handoff-give-gifter-side
            desc:handoff-give-gift-id

            <desc:handoff-receive>
            desc:handoff-receive
            desc:handoff-receive?
            desc:handoff-receive-receiver-exporter-session
            desc:handoff-receive-receiving-side
            desc:handoff-receive-handoff-count
            desc:handoff-receive-signed-give

            <op:start-session>
            op:start-session
            op:start-session?
            op:start-session-captp-version
            op:start-session-handoff-pubkey
            op:start-session-acceptable-location
            op:start-session-acceptable-location-sig

            marshallers
            unmarshallers

            signed-handoff-give?
            signed-handoff-receive?

            <internal-shutdown>
            internal-shutdown
            internal-shutdown?
            internal-shutdown-type
            internal-shutdown-reason

            <cmd-send-message>
            cmd-send-message
            cmd-send-message?
            cmd-send-message-msg

            <cmd-send-listen>
            cmd-send-listen
            cmd-send-listen?
            cmd-send-listen-listener-refr
            cmd-send-listen-wants-partial?

            <cmd-send-gc-answer>
            cmd-send-gc-answer
            cmd-send-gc-answer?
            cmd-send-gc-answer-answer-pos

            <cmd-send-gc-export>
            cmd-send-gc-export
            cmd-send-gc-export?
            cmd-send-gc-export-export-pos
            cmd-send-gc-export-wire-delta

            &mystery-exception
            make-mystery-exception
            mystery-exception?

            <question-finder>
            make-question-finder
            question-finder?
            question-finder-sealed-pos

            <giftmeta>
            make-giftmeta
            giftmeta?
            giftmeta-gift
            giftmeta-destroy-on-fetch

            <sessionmeta>
            make-sessionmeta
            sessionmeta?
            sessionmeta-location
            sessionmeta-local-bootstrap-obj
            sessionmeta-remote-bootstrap-obj
            sessionmeta-coordinator
            sessionmeta-session-name))

(define-record-type <captp-session-severed>
  (captp-session-severed)
  captp-session-severed?)

;;; Messages

;; Queue a delivery of verb(args..) to recip, discarding the outcome.
(define-syrup-record-type <op:deliver-only>
  (op:deliver-only to-desc args)
  op:deliver-only?
  op:deliver-only marshall::op:deliver-only unmarshall::op:deliver-only
  ;; Position in the table for the target
  ;; (sender's imports, reciever's exports)
  (to-desc op:deliver-only-to-desc)
   ;; Either arguments to the method or to the procedure, depending
   ;; on whether method exists
  (args op:deliver-only-args))

;; Queue a delivery of verb(args..) to recip, binding answer/rdr to the outcome.
(define-syrup-record-type <op:deliver>
  (op:deliver to-desc args answer-pos resolve-me-desc)
  op:deliver?
  op:deliver marshall::op:deliver unmarshall::op:deliver

  (to-desc op:deliver-to-desc)
  (args op:deliver-args)
  (answer-pos op:deliver-answer-pos)
  ;; a resolver, probably an import (though it could be a handoff)
  (resolve-me-desc op:deliver-resolve-me-desc))

(define-syrup-record-type <op:abort>
  (op:abort reason)
  op:abort?
  op:abort marshall::op:abort unmarshall::op:abort
  (reason op:abort-reason))

(define-syrup-record-type <op:listen>
  (op:listen to-desc listener-desc wants-partial?)
  op:listen?
  op:listen marshall::op:listen unmarshall::op:listen

  (to-desc op:listen-to-desc)
  (listener-desc op:listen-listener-desc)
  (wants-partial? op:listen-wants-partial?))

(define-syrup-record-type <op:gc-export>
  (op:gc-export export-pos wire-delta)
  op:gc-export?
  op:gc-export marshall::op:gc-export unmarshall::op:gc-export
  (export-pos op:gc-export-export-pos)
  (wire-delta op:gc-export-wire-delta))

(define-syrup-record-type <op:gc-answer>
  (op:gc-answer answer-pos)
  op:gc-answer?
  op:gc-answer marshall::op:gc-answer unmarshall::op:gc-answer
  (answer-pos op:gc-answer-answer-pos))

(define-syrup-record-type <desc:import-object>
  (desc:import-object pos)
  desc:import-object?
  desc:import-object marshall::desc:import-object unmarshall::desc:import-object
  (pos desc:import-object-pos))

(define-syrup-record-type <desc:import-promise>
  (desc:import-promise pos)
  desc:import-promise?
  desc:import-promise marshall::desc:import-promise unmarshall::desc:import-promise
  (pos desc:import-promise-pos))

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
(define-syrup-record-type <desc:export>
  (desc:export pos)
  desc:export?
  desc:export marshall::desc:export unmarshall::desc:export
  (pos desc:export-pos))

;; Something to answer that we haven't seen before.
;; As such, we need to set up both the promise import and this resolver/redirector
(define-syrup-record-type <desc:answer>
  (desc:answer pos)
  desc:answer?
  desc:answer marshall::desc:answer unmarshall::desc:answer
  (pos desc:answer-pos))

;; This is a general sig-envelope, we might have some more specific
;; ones.  Whatever signed must refer to another serializable record
;; which is concretely typed.  If not, we run into confused deputy,
;; replay, oracle attack possibilities.  See also:
;;   https://sandstorm.io/news/2015-05-01-is-that-ascii-or-protobuf
;; Note that the key is not referred to; if it isn't obvious by the
;; payload and the protocol, then we aren't doing things right.
(define-syrup-record-type <desc:sig-envelope>
  (desc:sig-envelope signed signature)
  desc:sig-envelope?
  desc:sig-envelope marshall::desc:sig-envelope unmarshall::desc:sig-envelope
  (signed desc:sig-envelope-signed)
  (signature desc:sig-envelope-signature))

;; Handoffs have three roles:
;;  - Gifter: who's sharing their import
;;  - Receiver: who's receiving the gift
;;  - Exporter: the location where the gift import is exported from
;;    (presumably, where it lives, though it may be a promise which
;;    eventually points to something else)

;; The handoff certificate from the gifter
(define-syrup-record-type <desc:handoff-give>
  (desc:handoff-give receiver-key exporter-location gifter-exporter-session gifter-side gift-id)
  desc:handoff-give?
  desc:handoff-give marshall::desc:handoff-give unmarshall::desc:handoff-give
   ;; handoff signing key this is being given to
   ;;   : handoff-key?
  (receiver-key desc:handoff-give-receiver-key)
   ;; exporter-location(-hint(s)): how to connect to get this
   ;;   : ocap-node-uri?
   ;;   Note that currently this requires a certain amount of VatTP
   ;;   crossover, since we have to give a way to connect to VatTP...
  (exporter-location desc:handoff-give-exporter-location)
   ;; session: which session betweein gifter and exporter at the location
   ;;   : bytes?
  (gifter-exporter-session desc:handoff-give-gifter-exporter-session)
   ;; gifter-side: which "named side" of the session is the gifter
   ;;   : bytes?
  (gifter-side desc:handoff-give-gifter-side)
  ;; gift-id: The gift id associated with this gift
   ;;   : (or/c integer? bytes?)
  (gift-id desc:handoff-give-gift-id))

;; TODO: Maybe we only need the receiving-side, unsure
(define-syrup-record-type <desc:handoff-receive>
  (desc:handoff-receive receiver-exporter-session receiving-side handoff-count signed-give)
  desc:handoff-receive?
  desc:handoff-receive marshall::desc:handoff-receive unmarshall::desc:handoff-receive
  (receiver-exporter-session desc:handoff-receive-receiver-exporter-session)
  (receiving-side desc:handoff-receive-receiving-side)
  (handoff-count desc:handoff-receive-handoff-count)
  (signed-give desc:handoff-receive-signed-give))

(define-syrup-record-type <op:start-session>
  (op:start-session captp-version handoff-pubkey acceptable-location acceptable-location-sig)
  op:start-session?
  op:start-session marshall::op:start-session unmarshall::op:start-session
  (captp-version op:start-session-captp-version)
  (handoff-pubkey op:start-session-handoff-pubkey)
  ;; a sig-envelope signed by handoff-pubkey with a <my-location $location-data>
  (acceptable-location op:start-session-acceptable-location)
  (acceptable-location-sig op:start-session-acceptable-location-sig))

;; TODO: 3 vat/node handoff versions (Promise3Desc, Far3Desc)

(define marshallers
  (list marshall::op:deliver-only
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

        marshall::ocapn-node
        marshall::ocapn-sturdyref))

(define unmarshallers
  (list unmarshall::op:deliver-only
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

        unmarshall::ocapn-node
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
(define-exception-type &mystery-exception &external-error
  make-mystery-exception
  mystery-exception?)

;; Question finders are a weird thing... we need some way to be able to
;; look up what question corresponds to an entry in the table.
;; Used by mactor:question (a special kind of promise),
;; since messages sent to a question are pipelined through the answer
;; side of some "remote" node.
(define-record-type <question-finder>
  (make-question-finder sealed-pos)
  question-finder?
  (sealed-pos question-finder-sealed-pos))

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
