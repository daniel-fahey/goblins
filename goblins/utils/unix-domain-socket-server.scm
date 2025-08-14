;;; Copyright 2025 Jessica Tallon
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

(define-module (goblins utils unix-domain-socket-server)
  #:use-module ((goblins) #:hide ($))
  #:use-module ((goblins core) #:select ($) #:prefix $)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib io)
  #:use-module (goblins actor-lib on)
  #:use-module (goblins utils base32)
  #:use-module (goblins utils crypto)
  #:use-module (goblins utils hashmap)
  #:use-module (goblins utils unix-domain-socket)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer unix-domain-socket)
  #:use-module (goblins contrib syrup)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:export (^unix-domain-socket-server
            unix-domain-socket-server-env))

(define (use-nonblocking-i/o port)
  (fcntl port F_SETFL (logior O_NONBLOCK (fcntl port F_GETFL)))
  port)

(define (non-blocking-accept sock)
  (accept sock SOCK_NONBLOCK))
(define (read-uds-msg sock)
  (syrup-read sock #:unmarshallers unmarshallers))
(define (write-uds-msg sock message)
  (syrup-write message sock #:marshallers marshallers))
(define (make-uds-msg-writer message)
  (lambda (sock)
    (write-uds-msg sock message)))

;; The general gist with this server is that OCapN peers will use it to get
;; introduced to other peers on the same machine. There is no federation or
;; mechanism to connect to peers which do not share an introduction server. The
;; peer initially sends a message announcing itself and sharing a challenge
;; which the server should sign to prove the server's key, the server asks the
;; client to do a similar challenge. After that requests for introductions can
;; occur.
;;
;; Introductions occur by the peer asking to be introduced to another peer by a
;; given OCapN location, alongside this it sends a unix domain socket to be
;; given to this peer. If the peer is not connected to this server, it sets up
;; a promise pair so that when the peer connects pending connections are
;; routed through it.
(define-actor (^unix-domain-socket-server bcom
                                          #:optional
                                          (privkey (generate-key-pair))
                                          #:key
                                          (max-clients 1024))
  #:portrait
  (lambda ()
    (list (private-key->data privkey)))
  #:restore
  (lambda (_version privkey-data)
    (spawn ^unix-domain-socket-server (data->private-key privkey-data)))

  (define peer->connection (spawn ^ghash (make-ocapn-peer-hashmap)))
  (define (handle-new-connection-requests peer-location client-io)
    "Handle listening to client-io providing introductions when requested to others"
    (define continue? (spawn ^cell #t))
    ;; Read a new request for introduction to a given peer.
    (define new-connection-vow
      (<- client-io 'read
          (lambda (sock)
            (match (read-uds-msg sock)
              (($ <uds:new-connection> (? ocapn-peer? from) (? ocapn-peer? to))
               (let ((forward-sock (read-port-from-socket sock)))
                 (list from to forward-sock)))
              ((? eof-object? eof) eof)))))

    (on new-connection-vow
        (match-lambda
          ;; When we get EOF do some cleanup...
          ((? eof-object?)
           ($$ continue? #f)
           (<-np client-io 'halt))
          ;; We have a new connection to forward.
          ((from to forward-sock)
           (unless (same-peer-location? from peer-location)
             (error (format #f "~a tried to open a new connection to ~a claiming to be ~a" peer-location to from)))
           (define dest-client
             (match ($$ peer->connection 'ref to)
               ;; If we (intro server) don't have a connection, setup a promise
               ;; pair and wait until they connect to us.
               [#f
                (let-values (((vow resolver) (spawn-promise-and-resolver)))
                  ($$ peer->connection 'set to (cons vow resolver))
                  vow)]
               ;; We already have a promsie pair setup...
               [(vow . resolver) vow]
               ;; The peer is connected to us, return the client.
               [(? live-refr? client) client]))
           ;; When the destination client resolves, send them the intro.
           (<-np dest-client 'write
                 (lambda (port)
                   (define new-conn-msg
                     (make-uds:new-connection from to))
                   (write-uds-msg port new-conn-msg)
                   (send-port-over-socket port forward-sock)))))
        #:catch
        (lambda (err)
          ($$ continue? #f))
        #:finally
        (lambda ()
          (when ($$ continue?)
            (handle-new-connection-requests peer-location client-io)))))

  (define (start-accepting-new-clients socket-io)
    "Listen for new peers connecting, verify them and listen for intro requests"
    (define client-io
      (on-match (<- socket-io non-blocking-accept)
        [(client . client-address)
         ;; Kick off the loop again
         (start-accepting-new-clients socket-io)
         ;; Make the client IO object.
         (spawn ^read-write-io (use-nonblocking-i/o client)
                               #:cleanup close-port)]))
    (define peer-location-vow
      (on-match (<- client-io 'read read-uds-msg)
        ;; First message a client sends, it comes with a challenge we must sign
        ;; and we need to also provide them with a challenge to verify they are
        ;; who they say they are.
        (($ <uds:register> (? ocapn-peer? peer-location) their-challenge)
         (let* ((peer-designator (ocapn-peer-designator peer-location))
                (peer-pubkey (bytevector->crypto-public-key (base32-decode peer-designator)))
                (their-challenge-sig (sign their-challenge privkey))
                (our-challenge (strong-random-bytes 64))
                (sendable-privkey (key-pair->public-key privkey))
                (response-msg (make-uds:server-response sendable-privkey
                                                        our-challenge
                                                        their-challenge-sig)))
           (<-np client-io 'write (make-uds-msg-writer response-msg))
           ;; Wait for the response to our challenge.
           (on-match (<- client-io 'read read-uds-msg)
             (($ <uds:signature> got-back-payload signature)
              (if (and (equal? got-back-payload our-challenge)
                       (verify signature our-challenge peer-pubkey))
                  peer-location
                  (error "Peer verification failed"))))))))
    ;; Once we've got here, we know our verification is complete
    ;; we just need to add them to the hashmap of peers.
    (on peer-location-vow
        (lambda (peer-location)
          ;; Kick off the loop which handles intros.
          (handle-new-connection-requests peer-location client-io)
          (match ($$ peer->connection 'ref peer-location)
            ;; If we've had intro requests to this peer, there will be a
            ;; promise pair waiting, fulfill that.
            ((vow . resolver)
             ($$ resolver 'fulfill client-io))
            ;; If we've got nothing, just add ourselves to the map.
            (#f
             ($$ peer->connection 'set peer-location client-io))))))

  (define (halt-me-beh socket-io)
    (lambda ()
      ;; Stop accepting new connections
      ($$ socket-io 'halt)
      ;; Go through each connection halting its IO actor and removing
      ;; it from our hashmap.
      (hashmap-for-each
       (lambda (peer client-io)
         ($$ client-io 'halt)
         ($$ peer->connection 'remove peer))
       ($$ peer->connection 'data))
      ;; Return back to the pre-setup behavior
      (bcom (^unix-domain-socket-server bcom privkey #:max-clients max-clients))))
  (lambda (sock addr)
    (define socket-io
      (spawn ^io (use-nonblocking-i/o sock)
             #:init
             (lambda (sock)
               (bind sock addr)
               (listen sock max-clients))
             #:cleanup close-port))
    (start-accepting-new-clients socket-io)
    (bcom (halt-me-beh socket-io))))

(define unix-domain-socket-server-env
  (persistence-env-compose
   (namespace-env (goblins utils unix-domain-socket-server)
     ^unix-domain-socket-server)
   unix-domain-socket-netlayer-env))
