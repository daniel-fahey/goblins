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

(define-module (tests ocapn netlayer test-unix-domain-socket)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib on)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn netlayer unix-domain-socket)
  #:use-module (goblins utils crypto)
  #:use-module (goblins utils base32)
  #:use-module (goblins utils hashmap)
  #:use-module (goblins utils unix-domain-socket)
  #:use-module (goblins utils unix-domain-socket-server)
  #:use-module (goblins persistence-store memory)
  #:use-module (tests utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(test-begin "test-unix-domain-socket")
(define server-vat (spawn-vat #:name "server"))

;; Utils
(define (spawn-unix-domain-socket-netlayer-and-mycapn . addresses)
  (define netlayer (spawn ^unix-domain-socket-netlayer))

  (for-each
    (lambda (address)
      (syscaller-free-fiber
       (lambda ()
         (define sock (make-unix-domain-socket))
         (connect sock address)
         (<-np-extern netlayer 'add-server sock))))
    addresses)
  (values netlayer (spawn-mycapn netlayer)))

(define* (spawn-uds-intro-server name #:key [key (generate-key-pair)])
  (define vat (spawn-vat #:name name))
  (define server-socket-path (tmpnam))
  (define server-socket-addr
    (make-socket-address AF_UNIX server-socket-path))
  (define server-socket (make-unix-domain-socket))

  ;; Spawn the server
  (with-vat vat
    (let ((uds-server (spawn ^unix-domain-socket-server key)))
      ($ uds-server server-socket server-socket-addr)
      (values vat uds-server server-socket-addr))))

(define (intro-server-key->name key)
  ;; The name is just the base32 encoded pubkey of a server
  (define pubkey (key-pair->public-key key))
  (define pubkey-bv (captp-public-key->bytevector pubkey))
  (base32-encode pubkey-bv))


;; Test can enliven over UDS netlayer
(define (test-enliven-over-uds-netlayer)
  (define peer1-vat (spawn-vat #:name "peer1"))
  (define peer2-vat (spawn-vat #:name "peer2"))

  ;; Setup the server
  (define-values (server-vat server server-socket-addr)
    (spawn-uds-intro-server "intro server"))

  ;; Connect peer1.
  (define-values (peer1-netlayer peer1-mycapn)
    (with-vat peer1-vat
      (spawn-unix-domain-socket-netlayer-and-mycapn server-socket-addr)))

  (define-values (peer2-netlayer peer2-mycapn)
    (with-vat peer2-vat
      (spawn-unix-domain-socket-netlayer-and-mycapn server-socket-addr)))

  (define peer2-cell-sref
    (with-vat peer2-vat
      (define cell (spawn ^cell 'yay))
      ($ peer2-mycapn 'register cell 'unix-domain-socket)))

  (resolve-vow-and-return-result
   peer1-vat
   (lambda ()
     (<- (<- peer1-mycapn 'enliven peer2-cell-sref)))))

(test-equal "Can enliven and call remote-refr over unix domain socket netlayer"
  #(ok yay)
  (test-enliven-over-uds-netlayer))

(define (test-using-multiple-introduction-servers)
  (define peer1-vat (spawn-vat #:name "peer1"))
  (define peer2-vat (spawn-vat #:name "peer2"))
  (define peer3-vat (spawn-vat #:name "peer3"))

  (define-values (server1-vat server1 server1-socket-addr)
    (spawn-uds-intro-server "intro server 1"))
  (define-values (server2-vat server2 server2-socket-addr)
    (spawn-uds-intro-server "intro server 2"))

  ;; Make peer1 initially connected to server1
  (define-values (peer1-netlayer peer1-mycapn)
    (with-vat peer1-vat
      (spawn-unix-domain-socket-netlayer-and-mycapn server1-socket-addr)))

  ;; Make peer2 connected to both server1 and server2
  (define-values (peer2-netlayer peer2-mycapn)
    (with-vat peer2-vat
      (spawn-unix-domain-socket-netlayer-and-mycapn
       server1-socket-addr
       server2-socket-addr)))

  ;; Make peer3 and connect it to server2
  (define-values (peer3-netlayer peer3-mycapn)
    (with-vat peer3-vat
      (spawn-unix-domain-socket-netlayer-and-mycapn server1-socket-addr)))

  (define cell1-sref
    (with-vat peer1-vat
      (let ((cell (spawn ^cell 'one)))
        ($ peer1-mycapn 'register cell 'unix-domain-socket))))
  (define cell3-sref
    (with-vat peer3-vat
      (let ((cell (spawn ^cell 'three)))
        ($ peer3-mycapn 'register cell 'unix-domain-socket))))

  (resolve-vow-and-return-result
   peer2-vat
   (lambda ()
     (let-on ((one (<- (<- peer2-mycapn 'enliven cell1-sref)))
              (three (<- (<- peer2-mycapn 'enliven cell3-sref))))
       (list one three)))))

(test-equal "Can enliven two refrs using two different intro servers"
  #(ok (one three))
  (test-using-multiple-introduction-servers))

(define (test-3ph-over-unix-domain-sockets)
  (define alice-vat (spawn-vat #:name "alice"))
  (define bob-vat (spawn-vat #:name "bob"))
  (define carol-vat (spawn-vat #:name "carol"))

  (define-values (server-vat server server-socket-addr)
    (spawn-uds-intro-server "intro server"))

  ;; Spawn the peers
  (define-values (alice-netlayer alice-mycapn)
    (with-vat alice-vat
      (spawn-unix-domain-socket-netlayer-and-mycapn server-socket-addr)))
  (define-values (bob-netlayer bob-mycapn)
    (with-vat bob-vat
      (spawn-unix-domain-socket-netlayer-and-mycapn server-socket-addr)))
  (define-values (carol-netlayer carol-mycapn)
    (with-vat carol-vat
      (spawn-unix-domain-socket-netlayer-and-mycapn server-socket-addr)))

  (define carol-sref
    (with-vat carol-vat
      (let ((cell (spawn ^cell 'carol)))
        ($ carol-mycapn 'register cell 'unix-domain-socket))))
  ;; Using a promise here instead of a cell so we can determine
  ;; when the handoff has happened.
  (define-values (bob-vow bob-resolver-sref)
    (with-vat bob-vat
      (define-values (vow resolver)
        (spawn-promise-and-resolver))
      (values vow
              ($ bob-mycapn 'register resolver 'unix-domain-socket))))

  (with-vat alice-vat
    (let-on ((carol (<- alice-mycapn 'enliven carol-sref))
             (bob (<- alice-mycapn 'enliven bob-resolver-sref)))
      (<- bob 'fulfill carol)))
  ;; Finally return the promise which sends a message to bob-vow. Since
  ;; alice fulfilled this with carol. The expected return value is 'carol
  (resolve-vow-and-return-result
   bob-vat
   (lambda ()
     (<- bob-vow))))

(test-equal "3PH work under UDS netlayer"
  #(ok carol)
  (test-3ph-over-unix-domain-sockets))

(define (test-communication-without-intro-server)
  (define peer1-vat (spawn-vat #:name "peer1"))
  (define peer2-vat (spawn-vat #:name "peer2"))

  (define-values (server-vat server server-socket-addr)
    (spawn-uds-intro-server "intro server 1"))

  ;; Peers
  (define-values (peer1-netlayer peer1-mycapn)
    (with-vat peer1-vat
      (spawn-unix-domain-socket-netlayer-and-mycapn server-socket-addr)))
  (define-values (peer2-netlayer peer2-mycapn)
    (with-vat peer2-vat
      (spawn-unix-domain-socket-netlayer-and-mycapn server-socket-addr)))

  (define-values (cell-sref cell)
    (with-vat peer2-vat
      (let ((cell (spawn ^cell)))
        (values ($ peer2-mycapn 'register cell 'unix-domain-socket)
                cell))))

  (define cell-vow
    (with-vat peer1-vat
      ($ peer1-mycapn 'enliven cell-sref)))

  ;; This test is going to work by once peer1 has got a cell refr
  ;; we're going to halt the vat containing the server, thus halting
  ;; the server. Once halted we're going to make a vow and resolver
  ;; pair. The vow will be the result of the test, the resolver will
  ;; go into the cell on peer2, peer1 will then get the resolver using
  ;; OCapN and send it to the fulfillment, verifying the connection
  ;; continues despite the server being halted.
  ;;
  ;; To ensure we don't run into timing issues we use a few different
  ;; promises.
  (define-values (resolver-in-cell-vow resolver-in-cell-resolver)
    (with-vat peer2-vat
      (spawn-promise-and-resolver)))

  (define result-vow
    (with-vat peer2-vat
      (on cell-vow
          (lambda _
            (define-values (vow resolver)
              (spawn-promise-and-resolver))
            ($ cell resolver)
            ;; Stop the intro server
            (vat-halt! server-vat)
            ($ resolver-in-cell-resolver 'fulfill #t)
            vow)
          #:promise? #t)))

  (with-vat peer1-vat
    (on resolver-in-cell-vow
        (lambda _
          (define resolver-vow (<- cell-vow))
          (<- resolver-vow 'fulfill 'it-worked))))
  (resolve-vow-and-return-result
   peer2-vat
   (lambda ()
     result-vow)))

(test-equal "UDS connections work even after the intro server stopped"
  #(ok it-worked)
  (test-communication-without-intro-server))

(define (test-uds-aurie-restore)
  (define intro-server-store (make-memory-store))
  (define peer1-store (make-memory-store))
  ;; This test is going to setup a UDS intro server and a peer.
  ;; Then we're going to restore both the peer and server from
  ;; aurie and connect to the sref from the pre-restore session

  ;; Setup the server initially.
  (define-values (server-vat server)
    (spawn-persistent-vat
     unix-domain-socket-server-env
     (lambda ()
       (spawn ^unix-domain-socket-server))
     intro-server-store))

  (define server-socket-path
    (tmpnam))
  (define server-socket-addr
    (make-socket-address AF_UNIX server-socket-path))
  (with-vat server-vat
    (let ((sock (make-unix-domain-socket)))
      ($ server sock server-socket-addr)))

  ;; Now setup the first peer
  (define peer-env
    (persistence-env-compose captp-env
                             unix-domain-socket-netlayer-env
                             cell-env))

  (define-values (peer1-vat peer1-cell peer1-netlayer peer1-mycapn)
    (spawn-persistent-vat
     peer-env
     (lambda ()
       (define netlayer (spawn ^unix-domain-socket-netlayer))
       (values (spawn ^cell)
               netlayer
               (spawn-mycapn netlayer)))
     peer1-store))

  (with-vat peer1-vat
    (define sock (make-unix-domain-socket))
    (syscaller-free-fiber
     (lambda ()
       (connect sock server-socket-addr)
       (<-np-extern peer1-netlayer 'add-server sock))))

  ;; Get the sref for the cell.
  ;; We're using the resolve-vow-and-return-result as we're going
  ;; to halt the vat below and it needs to have made the sref by
  ;; then.
  (define cell-sref
    (match (resolve-vow-and-return-result
            peer1-vat
            (lambda ()
              ($ peer1-mycapn 'register peer1-cell 'unix-domain-socket)))
      [#(ok sref) sref]))

  ;; Okay now lets stop the server and peer and respawn them.
  (vat-halt! server-vat)
  (vat-halt! peer1-vat)

  ;; the old ones will still be bound so make new addrs
  (define server-socket-path*
    (tmpnam))
  (define server-socket-addr*
    (make-socket-address AF_UNIX server-socket-path*))

  ;; Respawn
  (define-values (server-vat* server*)
    (spawn-persistent-vat
     unix-domain-socket-server-env
     (lambda ()
       (error "Should be restoring from the store"))
     intro-server-store))
  (with-vat server-vat*
    (let ((sock (make-unix-domain-socket)))
      ($ server* sock server-socket-addr*)))

  (define-values (peer1-vat* peer1-cell* peer1-netlayer* peer1-mycapn*)
    (spawn-persistent-vat
     peer-env
     (lambda ()
       (error "Should be restoring from the store"))
     peer1-store))

  (with-vat peer1-vat*
    (define sock (make-unix-domain-socket))
    (syscaller-free-fiber
     (lambda ()
       (connect sock server-socket-addr*)
       (<-np-extern peer1-netlayer* 'add-server sock))))

  ;; Now setup peer2 which will use the cell-sref setup
  ;; from the previous session to enliven the cell which
  ;; is now peer1-cell*.
  (define peer2-vat (spawn-vat #:name "peer2"))
  (define-values (peer2-netlayer peer2-mycapn)
    (with-vat peer2-vat
      (spawn-unix-domain-socket-netlayer-and-mycapn server-socket-addr*)))

  ;; Put something in the cell to make doubly sure it can't be
  ;; somehow referring the old cell on the halted vat, probably
  ;; not needed but just in case.
  (with-vat peer1-vat*
    ($ peer1-cell* 'aurie-works))

  (resolve-vow-and-return-result
   peer2-vat
   (lambda ()
     (<- (<- peer2-mycapn 'enliven cell-sref)))))

(test-equal "Aurie works on both UDS intro server and UDS netlayer"
  #(ok aurie-works)
  (test-uds-aurie-restore))

(define (test-disconnect-from-intro-server)
  (define peer1-vat (spawn-vat #:name "peer1"))
  (define peer2-vat (spawn-vat #:name "peer2"))
  (define peer3-vat (spawn-vat #:name "peer3"))

  (define-values (server-vat server server-socket-addr)
    (spawn-uds-intro-server "intro server"))

  ;; Peers
  (define-values (peer1-netlayer peer1-intro-server-sever-vow peer1-mycapn)
    (with-vat peer1-vat
      (let ((netlayer (spawn ^unix-domain-socket-netlayer))
            (sock (make-unix-domain-socket)))
        (connect sock server-socket-addr)
        (values netlayer
                ($ netlayer 'add-server sock)
                (spawn-mycapn netlayer)))))
  (define-values (peer2-netlayer peer2-mycapn)
    (with-vat peer2-vat
      (spawn-unix-domain-socket-netlayer-and-mycapn server-socket-addr)))
  (define-values (peer3-netlayer peer3-mycapn)
    (with-vat peer3-vat
      (spawn-unix-domain-socket-netlayer-and-mycapn server-socket-addr)))

  (define cell-sref
    (with-vat peer1-vat
      (let ((cell (spawn ^cell)))
        ($ peer1-mycapn 'register cell 'unix-domain-socket))))

  (define peer3-cell-sref
    (with-vat peer3-vat
      (let ((cell (spawn ^cell)))
        ($ peer3-mycapn 'register cell 'unix-domain-socket))))

  (define cell-vow
    (with-vat peer2-vat
      ($ peer2-mycapn 'enliven cell-sref)))

  ;; Once we're connected, lets shutdown the intro server and see if we get
  ;; notified of the severance...
  (with-vat server-vat
    (on cell-vow
        (lambda (cell-refr)
          ;; We're connected, lets halt this thing after setup the halt
          ;; behavior is invoked by sending it a message with no arguments.
          ($ server)
          ;; Annoyingly because of #803 we even after we've sent the halt things
          ;; don't immediately stop as there are a queue "tasks" within the IO
          ;; actor, the halt is just enqueued. To ensure this flushes, get peer1
          ;; to try and reach out and connect to peer3. Janky but should work.
          (<-np peer1-mycapn 'enliven peer3-cell-sref))))

  (resolve-vow-and-return-result
   peer1-vat
   (lambda ()
     peer1-intro-server-sever-vow)))

(test-equal "When netlayer receives disconnect from intro server. Sends message to promise"
  #(ok disconnect)
  (test-disconnect-from-intro-server))

(define (test-disconnect-from-intro-server-removes-hint)
  (define peer1-vat (spawn-vat #:name "peer1"))
  (define peer2-vat (spawn-vat #:name "peer2"))
  (define peer3-vat (spawn-vat #:name "peer3"))

  (define server1-key (generate-key-pair))
  (define server1-name (intro-server-key->name server1-key))
  (define-values (server1-vat server1 server1-socket-addr)
    (spawn-uds-intro-server "intro server 1" #:key server1-key))

  (define server2-key (generate-key-pair))
  (define server2-name (intro-server-key->name server2-key))
  (define-values (server2-vat server2 server2-socket-addr)
    (spawn-uds-intro-server "intro server 2" #:key server2-key))

  ;; Peers
  (define-values (peer1-netlayer peer1-server1-disconnect-vow peer1-mycapn)
    (with-vat peer1-vat
      (let ((netlayer (spawn ^unix-domain-socket-netlayer))
            (sock1 (make-unix-domain-socket))
            (sock2 (make-unix-domain-socket)))
        (connect sock1 server1-socket-addr)
        (connect sock2 server2-socket-addr)
        ;; We don't care about the disconnect vow from 2
        ($ netlayer 'add-server sock2)
        (values netlayer
                ($ netlayer 'add-server sock1)
                (spawn-mycapn netlayer)))))
  (define-values (peer2-netlayer peer2-mycapn)
    (with-vat peer2-vat
      (spawn-unix-domain-socket-netlayer-and-mycapn
       server1-socket-addr)))
  (define-values (peer3-netlayer peer3-mycapn)
    (with-vat peer3-vat
      (spawn-unix-domain-socket-netlayer-and-mycapn
       server1-socket-addr)))

  ;; We need to check both UDS servers get added to the hints
  ;; to begin with. This means we need two connections from peer1
  ;; which use both intro servers
  (define-values (cell-sref cell)
    (with-vat peer1-vat
      (let ((cell (spawn ^cell)))
        (values ($ peer1-mycapn 'register cell 'unix-domain-socket)
                cell))))

  (define peer3-cell-sref
    (with-vat peer3-vat
      (let ((cell (spawn ^cell)))
        ($ peer3-mycapn 'register cell 'unix-domain-socket))))

  (define cell-vow
    (with-vat peer2-vat
      ($ peer2-mycapn 'enliven cell-sref)))

  ;; Once connected, shutdown and then we can see what our locator is
  (define old-cell-sref-vow
    (with-vat server1-vat
      (let*-on ((remote-cell cell-vow)
                ;; Get it again to try and ensure it has both hints. Both UDS
                ;; servers might not have finished handshaking at the start.
                (cell-sref-again
                 (<- peer1-mycapn 'register cell 'unix-domain-socket)))
        ;; We're connected, lets halt this thing after setup the halt
        ;; behavior is invoked by sending it a message with no arguments.
        ($ server1)
        ;; Annoyingly because of #803 we even after we've sent the halt things
        ;; don't immediately stop as there are a queue "tasks" within the IO
        ;; actor, the halt is just enqueued. To ensure this flushes, get peer1
        ;; to try and reach out and connect to peer3. Janky but should work.
        (<-np peer1-mycapn 'enliven peer3-cell-sref)
        cell-sref-again)))

  (resolve-vow-and-return-result
   peer2-vat
   (lambda ()
     (let-on ((old-sref old-cell-sref-vow)
              (disconnect-reason peer1-server1-disconnect-vow)
              (new-sref (<- peer1-mycapn 'register cell 'unix-domain-socket)))
       (list server1-name server2-name
             (ocapn-peer-hints (ocapn-sturdyref-peer old-sref))
             (ocapn-peer-hints (ocapn-sturdyref-peer new-sref)))))))

;; This test doesn't work. Annoyingly there's a timing issue between the
;; disconnect handling and removing the UDS server from the hints. The
;; `new-sref' contains both hints as it's made before the netlayer manages to
;; remove the intro server from the hints...

;; (test-assert "Check hint is removed from UDS netlayer when intro server
;; disconnects"
;;   (match (test-disconnect-from-intro-server-removes-hint)
;;     (#(ok (server1-name server2-name old-hints new-hints))
;;      ;; The old hints were generated when both servers were connected and
;;      ;; should then have both names in the hints, the new should only have
;;      ;; server2's hints as server1 was disconnected.
;;      (and (hashmap-ref old-hints server1-name)
;;           (hashmap-ref old-hints server2-name)
;;           (hashmap-ref new-hints server2-name)
;;           ;; Check server1 hint does not exist.
;;           (let ((does-not-exist (cons 'no 'value)))
;;             (eq? (hashmap-ref new-hints server1-name does-not-exist)
;;                  does-not-exist))))))

(test-end "test-unix-domain-socket")
