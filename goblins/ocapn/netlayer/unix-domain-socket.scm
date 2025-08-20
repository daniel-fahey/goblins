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

(define-module (goblins ocapn netlayer unix-domain-socket)
  #:use-module ((goblins core) #:hide ($))
  #:use-module ((goblins core) #:select ($) #:prefix $)
  #:use-module (goblins vat)
  #:use-module (goblins define-actor)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib io)
  #:use-module (goblins actor-lib on)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins actor-lib inbox)
  #:use-module (goblins contrib syrup)
  #:use-module (goblins utils base32)
  #:use-module (goblins utils crypto)
  #:use-module (goblins utils hashmap)
  #:use-module (goblins utils unix-domain-socket)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer base-port)
  #:use-module (ice-9 match)
  #:export (^unix-domain-socket-netlayer
            unix-domain-socket-netlayer-env))

(define (use-nonblocking-i/o sock)
  (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL)))
  sock)
(define (read-uds-msg sock)
  (syrup-read sock #:unmarshallers unmarshallers))
(define (write-uds-msg sock message)
  (syrup-write message sock #:marshallers marshallers))
(define (make-uds-msg-writer message)
  (lambda (sock)
    (write-uds-msg sock message)))

(define-actor (^unix-domain-socket-netlayer bcom #:key
                                            (designator-key (generate-key-pair)))
  #:portrait
  (lambda ()
    (list (private-key->data designator-key)))
  #:restore
  (lambda (_version designator-key-data)
    (spawn ^unix-domain-socket-netlayer
           #:designator-key (data->private-key designator-key-data)))

  (define peer-designator
    (base32-encode
     (captp-public-key->bytevector
      (key-pair->public-key designator-key))))
  (define-values (our-loc-vow our-loc-resolver)
    (spawn-promise-and-resolver))

  ;; The unix domain socket netlayer uses hints to tell other peers which intro
  ;; servers are valid ways of reaching us. For each intro server we're
  ;; connected to we store the intro server name (base32 encoded pubkey). Since
  ;; new intro servers can be added at point and we can disconnect from intro
  ;; servers these may change and evolve over time, the following two procedures
  ;; add or remove an intro server from our hints.
  (define (remove-server-hint! server-pubkey-data)
    (define b32-server-pubkey
      (base32-encode server-pubkey-data))
    (on our-loc-vow
        (lambda (our-loc-cell)
          (let ((current-hints (ocapn-peer-hints ($$ our-loc-cell))))
            ($$ our-loc-cell
                (make-ocapn-peer 'unix-domain-socket peer-designator
                                 (hashmap-remove current-hints b32-server-pubkey)))))))

  (define (add-new-server-hint! server-pubkey-data)
    (define b32-server-pubkey
      (base32-encode server-pubkey-data))
    (if (near-promise-resolved? our-loc-vow)
        (let ((current-hints (ocapn-peer-hints ($$ our-loc-vow))))
          ($$ our-loc-vow
              (make-ocapn-peer 'unix-domain-socket peer-designator
                               (hashmap-set current-hints b32-server-pubkey #t))))
        ($$ our-loc-resolver 'fulfill
            (spawn ^cell
                   (make-ocapn-peer 'unix-domain-socket peer-designator
                                    (hashmap (b32-server-pubkey "t")))))))

  (define server-processes (spawn ^ghash))
  (define (add-server server-sock)
    ;; Make a promise pair, which will be fulfilled if we disconnect.
    (define-values (sever-vow sever-resolver)
      (spawn-promise-and-resolver))

    ;; Do initial handshake
    (define server-io
      (spawn ^read-write-io
             (use-nonblocking-i/o server-sock)
             #:cleanup
             close-port))
    ;; Make a secure challenge for the server to sign and send it!
    (define our-challenge (strong-random-bytes 64))
    ;; We have our-loc-vow, however we will resolve that after we've connected to
    ;; our first intro server. Since we need to give our location to the intro
    ;; server we make `our-loc' which will not have any hints yet and is used just
    ;; for this purpose.
    (define our-loc
      (make-ocapn-peer 'unix-domain-socket peer-designator #f))
    (<-np server-io 'write (make-uds-msg-writer
                            (make-uds:register our-loc our-challenge)))
    ;; When the server responds and we've verified add it to our proccesses list
    (on-match (<- server-io 'read read-uds-msg)
      ((? eof-object?)
       ($$ sever-resolver 'fulfill 'disconnect)
       ($$ server-io 'halt))
      (($ <uds:server-response> server-pubkey server-challenge our-challenge-sig)
       ;; First check the server signature matches their public key's signature
       (let ((server-crypto-pubkey (captp-public-key->crypto-public-key server-pubkey))
             (crypto-sig (captp-signature->crypto-signature our-challenge-sig)))
         (unless (verify crypto-sig our-challenge server-crypto-pubkey)
           ($$ sever-resolver 'fulfill 'error)
           (error "Server signature does not match pubkey provided")))

       (define server-pubkey-bv
         (captp-public-key->bytevector server-pubkey))
       (define b32-server-pubkey
         (base32-encode server-pubkey-bv))

       ;; Add the server to the list of hints.
       ($$ server-processes 'set b32-server-pubkey server-io)
       (add-new-server-hint! server-pubkey-bv)
       (accept-incoming-from-server server-io sever-resolver)

       ;; If we disconnect... ensure our hint is removed.
       (on sever-vow
           (lambda _
             (remove-server-hint! server-pubkey-bv)))

       ;; Now we've checked the server's pubkey, move on to providing the
       ;; signature for the challenge the server gave to us.
       (let* ((signature (sign server-challenge designator-key))
              (sig-bytes (captp-signature->crypto-signature signature))
              (uds-sig (make-uds:signature server-challenge sig-bytes)))
         (<-np server-io 'write (make-uds-msg-writer uds-sig))
         #t)))
    sever-vow)

  ;; Unlike most netlayers, because we can have connections from multiple UDS
  ;; servers, we need to accept incoming connections from several places too.
  ;; The `accept-incoming' procedure provided to the base-ports-netlayer expects
  ;; to call it once and get a promise which will resolve to a new connection
  ;; (socket). To achieve this while ensuring all sockets are listening,
  ;; including new ones added after the `accept-incoming' call, we use an
  ;; inbox/outbox where new connections are written to the outbox and
  ;; accept-incoming reads them from the inbox.
  (define-values (incoming-inbox incoming-outbox incoming-stop)
    (spawn-inbox))
  (define (accept-incoming-from-server server-io sever-resolver)
    (define new-conn-vow
      (<- server-io 'read
          (lambda (sock)
            (match (read-uds-msg sock)
              ((? eof-object? eof) eof)
              (($ <uds:new-connection> from to)
               (list from to (read-port-from-socket sock)))))))
    (on-match new-conn-vow
      ((? eof-object?)
       ($$ incoming-stop)
       ($$ sever-resolver 'fulfill 'disconnect))
      ((from to incoming-sock)
       (let-on ((our-loc (<- our-loc-vow)))
         (unless (same-peer-location? our-loc to)
           (error "Got new connection not meant for us" to))
         (<-np incoming-outbox (use-nonblocking-i/o incoming-sock))
         (accept-incoming-from-server server-io sever-resolver)))))
  (define (accept-incoming)
    (<- incoming-inbox))

  (define (connect-outgoing connect-to)
    ;; Try to find a server process that we can reach the peer on.
    (define server-io
      (hashmap-fold
       (lambda (b32-server-pubkey _ prev)
         (if prev
             prev
             (match ($$ server-processes 'ref b32-server-pubkey)
               [#f #f]
               [(? live-refr? server-io) server-io])))
       #f
       (ocapn-peer-hints connect-to)))

    (unless server-io
      (error "No server found to connect to peer" connect-to))

    ;; Make our anonymous socket pair and send that, returning
    ;; our side of the socket pair to CapTP.
    (match (socketpair PF_UNIX SOCK_STREAM 0)
      [(our-side . their-side)
       (let-on ((our-loc (<- our-loc-vow)))
         (<-np server-io 'write
               (lambda (port)
                 (define new-conn-msg
                   (make-uds:new-connection our-loc connect-to))
                 (write-uds-msg port new-conn-msg)
                 (send-port-over-socket port their-side))))
       (use-nonblocking-i/o our-side)]))

  (define base-port-netlayer-vow
    (let-on ((our-loc our-loc-vow))
      (spawn ^base-port-netlayer our-loc accept-incoming connect-outgoing)))
  (match-lambda*
   [('netlayer-name) 'unix-domain-socket]
   [('add-server server-sock) (add-server server-sock)]
   [args (apply <- base-port-netlayer-vow args)]))

(define unix-domain-socket-netlayer-env
  (persistence-env-compose
   (namespace-env (goblins ocapn netlayer unix-domain-socket)
     ^unix-domain-socket-netlayer)))
