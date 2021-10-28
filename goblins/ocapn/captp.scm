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
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins ocapn define-recordable)
  #:use-module (goblins ocapn structs-urls)
  #:use-module (ice-9 match)
  #:use-module (syrup)
  #:use-module (srfi srfi-9))

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
