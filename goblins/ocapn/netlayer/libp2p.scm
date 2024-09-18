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

(define-module (goblins ocapn netlayer libp2p)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 iconv)
  #:use-module (fibers channels)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins utils crypto)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib io)
  #:use-module (goblins actor-lib swappable)
  #:use-module (goblins actor-lib let-on)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer utils)
  #:use-module (goblins ocapn netlayer base-port)
  #:use-module (goblins contrib syrup)
  #:export (^libp2p-netlayer
            ocapn-node->libp2p-multiaddrs
            libp2p-multiaddress->ocapn-node
            libp2p-netlayer-env))

(define (libp2p-multiaddress->ocapn-node multiaddrs)
  (define peer-id
    (let* ((addr (car multiaddrs))
           (p2p-index (string-contains addr "/p2p/"))
           (ipfs-index (string-contains addr "/ipfs/"))
           (index (or p2p-index ipfs-index)))
      (if index
          (match (string-split (substring addr index) #\/)
            [(""  _p2p peer-id) peer-id]
            [something-else (error "Couldn't parse peer-id" something-else)]))))
  (define hints
    (map (lambda (addr) `(multiaddr ,addr)) multiaddrs))
  (make-ocapn-node 'libp2p peer-id hints))

(define (ocapn-node->libp2p-multiaddrs node)
  (unless (and (ocapn-node? node) (eq? (ocapn-node-transport node) 'libp2p))
    (error "Can only convert libp2p OCapN node to libp2p mutliaddrs"
           node))
  (map
   (lambda (addr)
     (match addr
       [('multiaddr multiaddr) multiaddr]))
   (ocapn-node-hints node)))

(define (libp2p-multiaddrs->libp2p-config multiaddrs)
  "Take a list of libp2p multi-addresses and remove the peer ID (i.e. /p2p/<peer-id>) from them"
  (map
   (lambda (addr)
     (let* ((p2p-index (string-contains addr "/p2p/"))
            (ipfs-index (string-contains addr "/ipfs/"))
            (index (or p2p-index ipfs-index)))
       (format #f
               "address:~a"
               (if index
                   (substring addr 0 index)
                   addr))))
   multiaddrs))


(define (build-filename . args)
  (string-join args file-name-separator-string))

(define default-libp2p-control-path
  (build-filename "/tmp" "libp2p-control.sock"))

(define default-libp2p-path
  (build-filename "/tmp" "goblins-libp2p"))

(define* (setup-ocapn-io control-path socket-dir
                         #:optional our-location private-key)
  ;; Set up the temporary directory and paths we'll be using for this
  ;; captp process
  (unless (file-exists? socket-dir)
    ;; TODO: Make this recursive?
    (mkdir socket-dir))
  (define incoming-connections-path
    (random-tmp-filename socket-dir
                         #:format-name
                         (lambda (name)
                           ;; store the uid in the ocapn sock directory so maybe
                           ;; we could add a gc routine for obviously-unused
                           ;; old sock files
                           (format #f "ocapn-~a-~a.sock"
                                   (getuid) name))))
  (define outgoing-connections-path
    (random-tmp-filename socket-dir
                         #:format-name
                         (lambda (name)
                           ;; store the uid in the ocapn sock directory so maybe
                           ;; we could add a gc routine for obviously-unused
                           ;; old sock files
                           (format #f "ocapn-~a-~a.sock"
                                   (getuid) name))))

  (define incoming-connections-sock
    (spawn ^io (make-server-unix-domain-socket incoming-connections-path)
           #:cleanup
           (lambda (resource)
             (close-port resource))))

  (define base-message
    (format #f "NEW incoming:~a outgoing:~a protocol:ocapn version:1.0.0"
            incoming-connections-path
            outgoing-connections-path))

  (define control-sock
    (spawn ^line-delimited-port (make-client-unix-domain-socket control-path)))

  (define private-key-config
    (if private-key
        (format #f "private-key:~a" private-key)
        ""))
  (define multiaddr-config
    (if our-location
        (let* ((multiaddrs (ocapn-node->libp2p-multiaddrs our-location))
               (address-config (libp2p-multiaddrs->libp2p-config multiaddrs)))
          (string-join address-config " "))
        ""))

  (define config-message
    (string-join (list base-message private-key-config multiaddr-config) " "))

  (<-np control-sock 'write-line config-message)

  (define (split-control-message message)
    (let* ((trimmed-message (string-trim message #\space))
           (pairs (string-split trimmed-message #\space)))
      (map (lambda (pair)
             (let ((separator-index (string-index pair #\:)))
               (list (substring pair 0 separator-index)
                     (substring pair (+ 1 separator-index)))))
           pairs)))
  
  (define-values (private-key-vow private-key-resolver)
    (spawn-promise-values))
  (define-values (our-location-vow our-location-resolver)
    (spawn-promise-values))

  (on (<- control-sock 'read-line)
      (lambda (message)
        (match (split-control-message message)
          [(("address" multiaddrs) ... ("private-key" privkey))
           ($ private-key-resolver 'fulfill privkey)
           ($ our-location-resolver 'fulfill
              (libp2p-multiaddress->ocapn-node multiaddrs))]
          [something
           ($ private-key-resolver 'break (format #f "Expected private key, got ~a" something))
           ($ our-location-resolver 'break (format #f "Expected our-location, got ~a" something))
           (error "Got unknown response from libp2p daemon:" something)])))

  (values our-location-vow private-key-vow
          control-sock
          incoming-connections-sock
          outgoing-connections-path))

(define (setup-outgoing-sock sock location)
  (display
   (format #f "CONNECT ~a\n"
           (string-join (ocapn-node->libp2p-multiaddrs location) " "))
   sock)
  (flush-output-port sock))

(define-actor (^libp2p-netlayer-SETUP bcom
                                      our-location private-key
                                      control-path path
                                      #:optional
                                      init-control-sock
                                      init-incoming-connection-sock
                                      init-outgoing-connection-path)
  #:portrait (lambda ()
               (list our-location private-key control-path path))

  (define-values (_our-location _private-key
                                control-sock
                                incoming-connection-sock
                                outgoing-connection-path)
    (if (and init-control-sock init-incoming-connection-sock init-outgoing-connection-path)
        (values #f #f init-control-sock
                init-incoming-connection-sock
                init-outgoing-connection-path)
        (setup-ocapn-io control-path path our-location private-key)))

  (define (incoming-accept)
    (on (<- incoming-connection-sock
            (lambda (port)
              (accept port O_NONBLOCK)))
        (match-lambda
          ((client . addr)
           (setvbuf client 'block 1024)
           client))
        #:promise? #t))

  (define (outgoing-connect-location location)
    (unless (eq? (ocapn-node-transport location) 'libp2p)
      (error "Wrong netlayer! Expected libp2p" location))
    (let ((designator (ocapn-node-designator location))
          (sock (make-client-unix-domain-socket outgoing-connection-path)))
      (setup-outgoing-sock sock location)
      sock))
  (^base-port-netlayer bcom our-location
                       incoming-accept outgoing-connect-location))

(define-actor (^libp2p-netlayer-FRESH _bcom swap-to control-path path)
  ;; We're not fully setup yet so setup a promise pair to forward messages
  ;; sent to us while we're setting ourselves up.
  (define-values (setup-netlayer-vow setup-netlayer-resolver)
    (spawn-promise-values))

  (define-values (our-location-vow private-key-vow
                                   control-sock
                                   incoming-connection-sock
                                   outgoing-connection-path)
    (setup-ocapn-io control-path path))

  (let-on ((our-location our-location-vow)
           (private-key private-key-vow))
          (let ((ready (spawn ^libp2p-netlayer-SETUP
                              our-location private-key
                              control-path path
                              control-sock
                              incoming-connection-sock
                              outgoing-connection-path)))
            (<-np swap-to ready)
            (<-np setup-netlayer-resolver 'fulfill ready)))

  (lambda args
    (match args
      [('netlayer-name) 'libp2p]
      [args (apply <- setup-netlayer-vow args)])))

(define* (^libp2p-netlayer _bcom
                           #:optional private-key our-location
                           #:key
                           [control-path default-libp2p-control-path]
                           [path default-libp2p-path])
  (define-values (swap-to-vow swap-to-resolver)
    (spawn-promise-values))

  (define netlayer
    (if (and our-location private-key)
        (spawn ^libp2p-netlayer-SETUP our-location private-key
               control-path path)
        (spawn ^libp2p-netlayer-FRESH swap-to-vow
               control-path path)))

  (define-values (proxy swap-to)
    (swappable netlayer '^libp2p-netlayer))
  ($ swap-to-resolver 'fulfill swap-to)
  proxy)

(define libp2p-netlayer-env
  (make-persistence-env
   `((((goblins ocapn netlayer libp2p) ^libp2p-netlyaer-FRESH) ,^libp2p-netlayer-FRESH)
     (((goblins ocapn netlayer libp2p) ^libp2p-netlayer-SETUP) ,^libp2p-netlayer-SETUP))
   #:extends swappable-env))
