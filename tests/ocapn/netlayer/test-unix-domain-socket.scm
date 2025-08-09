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
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn netlayer unix-domain-socket)
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

;; Test can enliven over UDS netlayer
(define (test-enliven-over-uds-netlayer)
  (define server-vat (spawn-vat #:name "server"))
  (define peer1-vat (spawn-vat #:name "peer1"))
  (define peer2-vat (spawn-vat #:name "peer2"))

  ;; Setup the server
  (define server-socket-path
    (tmpnam))
  (define server-socket-addr
    (make-socket-address AF_UNIX server-socket-path))
  (define server
    (with-vat server-vat
      (let ((uds-server (spawn ^unix-domain-socket-server))
            (sock (make-unix-domain-socket)))
        ($ uds-server sock server-socket-addr)
        uds-server)))

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
  (define server1-vat (spawn-vat #:name "server1"))
  (define server2-vat (spawn-vat #:name "server2"))

  (define peer1-vat (spawn-vat #:name "peer1"))
  (define peer2-vat (spawn-vat #:name "peer2"))
  (define peer3-vat (spawn-vat #:name "peer3"))

  (define server1-socket-path
    (tmpnam))
  (define server1-socket-addr
    (make-socket-address AF_UNIX server1-socket-path))
  (define server1
    (with-vat server1-vat
      (let ((uds-server (spawn ^unix-domain-socket-server))
            (sock (make-unix-domain-socket)))
        ($ uds-server sock server1-socket-addr)
        uds-server)))

  (define server2-socket-path
    (tmpnam))
  (define server2-socket-addr
    (make-socket-address AF_UNIX server2-socket-path))
  (define server2
    (with-vat server2-vat
      (let ((uds-server (spawn ^unix-domain-socket-server))
            (sock (make-unix-domain-socket)))
        ($ uds-server sock server2-socket-addr)
        uds-server)))

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
      (spawn-unix-domain-socket-netlayer-and-mycapn server2-socket-addr)))

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
  (define server-vat (spawn-vat #:name "server"))
  (define alice-vat (spawn-vat #:name "alice"))
  (define bob-vat (spawn-vat #:name "bob"))
  (define carol-vat (spawn-vat #:name "carol"))

  (define server-socket-path
    (tmpnam))
  (define server-socket-addr
    (make-socket-address AF_UNIX server-socket-path))

  ;; Spawn the server
  (define server
    (with-vat server-vat
      (let ((uds-server (spawn ^unix-domain-socket-server))
            (sock (make-unix-domain-socket)))
        ($ uds-server sock server-socket-addr)
        uds-server)))

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
  (define server-vat (spawn-vat #:name "server"))

  (define peer1-vat (spawn-vat #:name "peer1"))
  (define peer2-vat (spawn-vat #:name "peer2"))

  (define server-socket-path
    (tmpnam))
  (define server-socket-addr
    (make-socket-address AF_UNIX server-socket-path))

  ;; Spawn the server
  (define server
    (with-vat server-vat
      (let ((uds-server (spawn ^unix-domain-socket-server))
            (sock (make-unix-domain-socket)))
        ($ uds-server sock server-socket-addr)
        uds-server)))

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

  ;; Put something in the cell to make doubley sure it can't be
  ;; somehow refercing the old cell on the halted vat, probably
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

(test-end "test-unix-domain-socket")
