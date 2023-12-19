;;; Copyright 2023 Christine Lemmer-Webber
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

(define-module (goblins ocapn netlayer prelay)
  #:use-module (ice-9 match)
  #:use-module (fibers channels)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins inbox)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib facet)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins utils base32)
  #:use-module (goblins contrib syrup)
  #:export (prelay-sturdyref->prelay-node
            prelay-node->prelay-sturdyref
            spawn-prelay-pair
            ^prelay-netlayer))



;;; URI utils
;;; =========

(define (prelay-sturdyref->prelay-node prelay-endpoint-sref)
  "Convert PRELAY-ENDPOINT-SREF sturdyref into an ocapn node

This sturdyref represents the underlying prelay endpoint."
  (let* ((swiss-num (ocapn-sturdyref-swiss-num prelay-endpoint-sref))
         (node (ocapn-sturdyref-node prelay-endpoint-sref))
         (orig-transport (ocapn-node-transport node))
         (orig-designator (ocapn-node-designator node))
         (orig-hints (ocapn-node-hints node))
         (encoded-designator
          (base32-encode
           (syrup-encode (list orig-designator
                               orig-transport
                               swiss-num)))))
    (make-ocapn-node 'prelay
                     encoded-designator
                     orig-hints)))

(define (prelay-node->prelay-sturdyref prelay-node)
  "Convert PRELAY-NODE into an OCapN sturdyref

This sturdyref represents the underlying prelay endpoint."
  (let ((netlayer-name (ocapn-node-transport prelay-node)))
    (unless (eq? netlayer-name 'prelay)
      (error "Attempt to connect to non-prelay node via prelay netlayer:"
             netlayer-name)))
  (match (syrup-decode (base32-decode (ocapn-node-designator prelay-node)))
    ((designator netlayer-name swiss-num)
     (let ((reconstructed-node
            (make-ocapn-node netlayer-name
                             designator
                             (ocapn-node-hints prelay-node))))
       (make-ocapn-sturdyref reconstructed-node
                             swiss-num)))))


;;; PRELAY CODE
;;; ===========

;; This actually lives *on* the relay vat... ie, it *is* the relay.
;; TODO: This needs to handle all the severance stuff when a disconnect
;;   happens or a new deliver-to is set up
(define (spawn-prelay-pair enliven)
  "Spawn a pair of prelay objects: the endpoint (public) and controller (private)

ENLIVEN is a capability to enliven a sturdyref, provided with the method 'enliven
and a sturdyref, it should return a promise.
Probably a facet of the MyCapN object.

Returns two values to its continuation, the ENDPOINT and CONTROLLER
respectively."
  (define-cell client-session-listener #f)
  (define (^prelay-endpoint bcom)
    (lambda (their-prelay-in)
      (unless ($ client-session-listener)
        (error "Prelay endpoint not configured!"))
      (define our-prelay-out (spawn ^session-prelay-out their-prelay-in))
      (define our-prelay-in-vow
        (on (<- ($ client-session-listener) 'incoming-session our-prelay-out)
            (lambda (client-deliver-incoming)
              (spawn ^session-prelay-in client-deliver-incoming))
            #:promise? #t))
      our-prelay-in-vow))
  (define (^prelay-controller _bcom)
    (methods
     ;; Connect to TO-ENDPOINT, which can be a live ref or a sturdyref
     ((connect to-endpoint deliver-in)
      ;; TODO: Do we allow deliver-in
      ;; refr or is it always a sturdyref?
      ;; TODO: Do we need to transform this node?
      (define to-endpoint-vow
        (<- enliven 'enliven (prelay-node->prelay-sturdyref to-endpoint)))
      (define our-prelay-in
        (spawn ^session-prelay-in deliver-in))
      (define their-prelay-in-vow
        (<- to-endpoint-vow our-prelay-in))
      ;; [Note from cwebber, 2023-07-28:]
      ;; I'm more confident about doing this particular `on' for message
      ;; ordering semantics, though it may have been fine to hand the
      ;; ^session-prelay-out their-prelay-in-vow... it maybe is!  I'm
      ;; just doing too much at once to carefully analyze for sure,
      ;; and ordering guarantees here is pretty essential
      (define our-prelay-out-vow
        (on their-prelay-in-vow
            (lambda (their-prelay-in)
              (spawn ^session-prelay-out their-prelay-in))
            #:promise? #t))
      ;; Hand back the promise which will allow for sending messages
      ;; once everything is correctly configured
      our-prelay-out-vow)
     ((set-session-listener session-listener)
      ($ client-session-listener session-listener))))
  (values (spawn ^prelay-endpoint)
          (spawn ^prelay-controller)))

(define (^session-prelay-in bcom deliver-in)
  "Constructs a PRELAY-IN object, used to send objects to us"
  (define base-beh
    (methods
     ;; Pass along message to user's deliver-in
     ((deliver encoded-message)
      (<-np deliver-in 'deliver encoded-message))
     ;; Halt the connection.
     ;; TODO: Needs work.
     ((abort)
      (<-np deliver-in 'abort)
      (bcom closed-beh))))
  (define closed-beh
    (lambda _ (error "Prelay session closed")))
  base-beh)

(define (^session-prelay-out bcom their-prelay-in)
  "Constructs a PRELAY-OUT object, which we use to send to the other side"
  (define base-beh
    (methods
     ((deliver encoded-message)
      (<-np their-prelay-in 'deliver encoded-message))
     ((abort)
      (<-np their-prelay-in 'abort)
      (bcom closed-beh))))
  (define closed-beh
    (lambda _ (error "Prelay session closed")))
  base-beh)



;;; CLIENT CODE
;;; ===========

;; This lives on the "prelay client"

;; We're going to hand the connection establisher, as usual for
;; netlayers, a (blocking) method to read messages and a (blocking)
;; method to write messages.  The connection establisher informs
;; our mycapn of our interest in establishing a new connection,
;; which our mycapn does for us.
;;
;; We're going to do this via two means:
;;
;;  - Incoming messages: An actor which receives messages over
;;    ordinary ocapn abstracted over some other netlayer we're
;;    using.  This actor then sends a message to an intermediary
;;    fibers "inbox" (delivery agent from `(goblins inbox)').
;;    The `read-message' procedure we end up finally setting up
;;    reads from this channel so that the actor can deliver
;;    messages to a blocking interface without itself blocking
;;  - Outgoing messages: These are much easier, as we can simply
;;    serialize the message and fire it off to the remote actor.
(define (^prelay-netlayer bcom prelay-endpoint-sref prelay-controller)
  "Constructs the prelay netlayer which lives on the client.

Takes three arguments at spawn time:
 - PRELAY-ENDPOINT-SREF: Sturdyref of the endpoint we will use to
   communicate with
 - PRELAY-CONTROLLER: Live, probably remote, reference which we use to
   pilot the prelay we communicate through."
  (define our-location
    (prelay-sturdyref->prelay-node prelay-endpoint-sref))

  (define (start-listener conn-establisher)
    (define (^session-listener _bcom)
      ;; This is where an incoming session comes in...
      (methods
       ((incoming-session session-prelay-outgoing)
        (define-values (client-deliver-in incoming-deq-ch incoming-stop?)
          (setup-delivery-agent-and-actor))
        (define-values (read-message write-message)
          (make-read-write-message incoming-deq-ch session-prelay-outgoing))
        (<-np conn-establisher
              read-message write-message
              #f)
        ;; Now we need to return the client-deliver-in
        client-deliver-in)))
    (define listener (spawn ^session-listener))
    ;; Tell the remote endpoint that we're looking for incoming connections
    (<-np prelay-controller 'set-session-listener listener))

  (define base-beh
    (methods
     ((netlayer-name) 'prelay)
     ((our-location) our-location)))
  (define pre-setup-beh
    (extend-methods base-beh
      ((setup conn-establisher)
       (start-listener conn-establisher)
       ;; Now that we're set up, transition to the main behavior
       (bcom (ready-beh conn-establisher)))))
  (define (ready-beh conn-establisher)
    (extend-methods base-beh
      ((self-location? loc)
       (same-node-location? our-location loc))
      ((connect-to remote-node)
       ;;;; Commented out because the relay is going to do some key authentication
       ;;;; checks later so it needs the node itself... but we're going to need
       ;;;; need to do that too presumably, so maybe we'll uncomment this once we
       ;;;; add cryptography
       ;; ;; Now we use our relay controller to connect to the object
       ;; ;; First that means deconstructing the URI so we can get out
       ;; ;; the relevant sturdyref object
       ;; (define remote-endpoint-sref (relay-node->relay-sturdyref remote-node))
       ;; ;; Now we need to enliven it
       ;; (define remote-endpoint-vow (enliven remote-endpoint-sref))
       (define-values (deliver-in incoming-deq-ch incoming-stop?)
         (setup-delivery-agent-and-actor))

       ;; TODO: Should we be giving just the sturdyref to the endpoint
       ;;   or the remote-node?  I'm not sure.
       (on (<- prelay-controller 'connect remote-node deliver-in)
           (lambda (session-prelay-outgoing)
             (define-values (read-message write-message)
               (make-read-write-message incoming-deq-ch session-prelay-outgoing))
             (<- conn-establisher
                 read-message
                 write-message
                 remote-node))
           #:promise? #t))))
  pre-setup-beh)

;;; Helpers for the ^prelay-controller machinery.
;;; The following code is used for both outgoing and incoming connections
;;; but is used a little bit differently.

;; Set up the client-deliver-in actor as well as the inbox/outbox
;; queues used to communicate between it and the read-message procedure
;; which we will hand to the connection establisher
(define (setup-delivery-agent-and-actor)
  ;; So, the first thing we need to do... let's set up the inbox...
  (define-values (incoming-enq-ch incoming-deq-ch incoming-stop?)
    (spawn-delivery-agent))
  ;; Now we need to set up our delivery proxy which will receive
  ;; messages (since we're going to need to give it to the other
  ;; endpoint)
  (define (^client-deliver-in bcom)
    (define main-beh
      (methods
       ((deliver encoded-message)
        ;; We don't decode the message ourselves at this point, we let
        ;; the other side of the fiber, which has the unmarshallers, do
        ;; that
        (put-message incoming-enq-ch encoded-message)
        ;; No significant value returned (but don't want a zero valued
        ;; continuation error)
        *unspecified*)
       ((abort)
        (put-message incoming-enq-ch the-eof-object)
        (bcom closed-beh))))
    (define closed-beh
      (lambda _ (error "Prelay session closed")))
    main-beh)
  (define client-deliver-in (spawn ^client-deliver-in))
  (values client-deliver-in incoming-deq-ch incoming-stop?))


;; These two procedures are actually used by the syscaller-free fibers
;; that the mycapn / conn-establisher set up which run in their own
;; loop.  So this is *not* happening in a Goblins context, and these
;; two will block
(define (make-read-write-message incoming-deq-ch session-prelay-outgoing)
  (define (read-message unmarshallers)
    (define encoded-message (get-message incoming-deq-ch))
    (syrup-decode encoded-message #:unmarshallers unmarshallers))
  (define (write-message msg marshallers)
    (define encoded-msg (syrup-encode msg #:marshallers marshallers))
    (<-np-extern session-prelay-outgoing 'deliver encoded-msg))
  (values read-message write-message))
