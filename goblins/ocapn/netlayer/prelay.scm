;;; Copyright 2023 Christine Lemmer-Webber
;;; Copyright 2024 Jessica Tallon
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
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins inbox)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib facet)
  #:use-module (goblins actor-lib io)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins utils base32)
  #:use-module (goblins contrib syrup)
  #:export (prelay-sturdyref->prelay-node
            prelay-node->prelay-sturdyref
            spawn-prelay-pair
            ^prelay-netlayer
            prelay-env))


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


;;; Other utilities
;;; ===============

;; This works like a cell like the name where by you can get the value
;; with no arguments, and set a new value with a value. It differs in
;; that when spawned with no initial value (how it's used), it'll
;; create a promise which it'll give out, when the value is set, it'll
;; then fulfill that promise with that value.
;;
;; It's used below as a sort of promise which can be resolved multiple
;; times.
(define-actor (^promise-cell bcom #:optional initial-value)
  ;; There are two main reasons we don't want aurie to persist the
  ;; value of this cell:
  ;; 1. It'll almost always either be an unresolved promise or
  ;;    remote-refr, neither of which are actually depictable (yet).
  ;; 2. This is used in the prelay netlayer and should be setup anew
  ;;    each time anyway.
  #:portrait (lambda () '())
  
  (define-values (initial-vow initial-resolver)
    (spawn-promise-and-resolver))

  (define current-value
    (if initial-value
        initial-value
        initial-vow))

  (case-lambda
    (() current-value)
    ((new-value)
     (if (eq? current-value initial-vow)
         (begin
           ($ initial-resolver 'fulfill new-value)
           (bcom (^promise-cell bcom new-value)))
         (bcom (^promise-cell bcom new-value))))))

(define-actor (^swappable-forwarder bcom send-to)
  (lambda args
    (apply <- ($ send-to) args)))

;; A little utility that maybe, possibly, could be useful for other
;; things and might be worth moving out.  Might be worth supporting
;; both "fulfilled" and "broken" resolutions in that case.
(define (spawn-swappable-promise-pair)
  "Spawn a forwarder, which mostly works like a promise, and a resolver,
which is like a promise resolver which can only be fulfilled, but can
be fulfilled more than once"
  (define send-to (spawn-named 'send-to ^promise-cell))
  (values (spawn ^swappable-forwarder send-to) send-to))



;;; PRELAY CODE
;;; ===========

;; This actually lives *on* the relay vat... ie, it *is* the relay.
;; TODO: This needs to handle all the severance stuff when a disconnect
;;   happens or a new deliver-to is set up
(define-actor (^prelay-endpoint _bcom client-session-listener)
  (lambda (their-prelay-in)
    (define our-prelay-out (spawn ^session-prelay-out their-prelay-in))
    (define our-prelay-in-vow
      (on (<- client-session-listener 'incoming-session our-prelay-out)
          (lambda (client-deliver-incoming)
            (spawn ^session-prelay-in client-deliver-incoming))
          #:promise? #t))
    our-prelay-in-vow))

(define-actor (^prelay-controller _bcom enliven client-session-listener-resolver)
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
    ($ client-session-listener-resolver session-listener))))

(define (spawn-prelay-pair enliven)
  "Spawn a pair of prelay objects: the endpoint (public) and controller (private)

ENLIVEN is a capability to enliven a sturdyref, provided with the method 'enliven
and a sturdyref, it should return a promise.
Probably a facet of the MyCapN object.

Returns two values to its continuation, the ENDPOINT and CONTROLLER
respectively."
  (define-values (client-session-listener
                  client-session-listener-resolver)
    (spawn-swappable-promise-pair))

  (values (spawn ^prelay-endpoint client-session-listener)
          (spawn ^prelay-controller enliven client-session-listener-resolver)))

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
(define-actor (^prelay-netlayer* bcom self enliven
                                 prelay-endpoint-sref-vow
                                 prelay-controller-sref-vow)
  (define our-location-vow
    (on prelay-endpoint-sref-vow
        (lambda (prelay-endpoint-sref)
          (prelay-sturdyref->prelay-node prelay-endpoint-sref))
        #:promise? #t))

  (define prelay-controller
    (<- enliven 'enliven prelay-controller-sref-vow))

  (define (start-listener conn-establisher)
    (define (^session-listener _bcom)
      ;; This is where an incoming session comes in...
      (methods
       ((incoming-session session-prelay-outgoing)
        (define-values (client-deliver-in incoming-deq-ch incoming-stop?)
          (setup-delivery-agent-and-actor))
        (define message-io
          (spawn-message-io incoming-deq-ch session-prelay-outgoing))
        (<-np conn-establisher message-io #f)
        ;; Now we need to return the client-deliver-in
        client-deliver-in)))
    (define listener (spawn ^session-listener))
    ;; Tell the remote endpoint that we're looking for incoming connections
    (<-np prelay-controller 'set-session-listener listener))

  (define-values (setup-netlayer-vow setup-netlayer-resolver)
    (spawn-promise-and-resolver))

  (on our-location-vow
      (lambda (our-location)
        ($ setup-netlayer-resolver 'fulfill (spawn ^netlayer our-location))))

  (define-values (conn-establisher-vow conn-establisher-resolver)
    (spawn-promise-and-resolver))

  (define (^netlayer _bcom our-location)
    (methods
     ((netlayer-name) 'prelay)
     ((our-location) our-location)
     ((self-location? loc)
      (same-node-location? our-location loc))
     ((setup conn-establisher)
      (start-listener conn-establisher)
      ;; Now that we're set up, transition to the main behavior
      (<-np conn-establisher-resolver 'fulfill conn-establisher))
     ((connect-to remote-node)
      ;; Commented out because the relay is going to do some key authentication
      ;; checks later so it needs the node itself... but we're going to need
      ;; need to do that too presumably, so maybe we'll uncomment this once we
      ;; add cryptography
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
            (define message-io
              (spawn-message-io incoming-deq-ch session-prelay-outgoing))
            (<- conn-establisher-vow message-io remote-node))
          #:promise? #t))))
  (lambda args
    (match args
      [('netlayer-name) 'prelay]
      [args
       (apply <- setup-netlayer-vow args)])))

(define (^prelay-netlayer _bcom enliven controller-sref endpoint-sref)
    "Constructs the prelay netlayer which lives on the client.

Takes three arguments at spawn time:
 - ENLIVEN: is a capability to enliven a sturdyref, provided with the method 'enliven
   and a sturdyref, it should return a promise.
   Probably a facet of the MyCapN object.
 - PRELAY-ENDPOINT-SREF: Sturdyref of the endpoint we will use to
   communicate with
 - PRELAY-CONTROLLER-SREF: Strudyref of the controller which we use to
   pilot the prelay we communicate through."
  (define self (spawn ^cell))
  (define netlayer (spawn ^prelay-netlayer* self enliven controller-sref endpoint-sref))
  ($ self netlayer)
  netlayer)

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

(define (spawn-message-io incoming-deq-ch session-prelay-outgoing)
  (define incoming (spawn ^io incoming-deq-ch))
  (define (^message-io _bcom)
    (methods
     [(read-message unmarshallers)
      ($ incoming
         (lambda (ch)
           (syrup-decode (get-message ch) #:unmarshallers unmarshallers)))]
     [(write-message msg marshallers)
      (define encoded-msg (syrup-encode msg #:marshallers marshallers))
      (<-np session-prelay-outgoing 'deliver encoded-msg)]))
  (spawn ^message-io))

(define prelay-env
  (make-persistence-env
   `((((goblins ocapn netlayer prelay) ^swappable-forwarder) ,^swappable-forwarder)
     (((goblins ocapn netlayer prelay) ^promise-cell) ,^promise-cell)
     (((goblins ocapn netlayer prelay) ^prelay-endpoint) ,^prelay-endpoint)
     (((goblins ocapn netlayer prelay) ^prelay-controller) ,^prelay-controller)
     (((goblins ocapn netlayer prelay) ^prelay-netlayer) ,^prelay-netlayer*))
   #:extends (list cell-env facet-env)))
