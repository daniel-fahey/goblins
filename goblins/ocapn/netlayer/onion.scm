;;; Copyright 2021-2022 Christine Lemmer-Webber
;;; Copyright 2022-2024 Jessica Tallon
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

(define-module (goblins ocapn netlayer onion)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 iconv)
  #:use-module (srfi srfi-11)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (fibers channels)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib swappable)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib io)
  #:use-module (goblins actor-lib on)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer utils)
  #:use-module (goblins ocapn netlayer onion-socks)
  #:use-module (goblins ocapn netlayer base-port)
  #:use-module (goblins contrib syrup)
  #:export (^onion-netlayer
            onion-netlayer-env))

(define (spawn-tor-control-connect-unix path)
  (define sock
    (make-client-unix-domain-socket path))
  (spawn ^line-delimited-port sock))

(define (build-path . args)
  (string-join args file-name-separator-string))

(define default-goblins-tor-dir
  (build-path (getenv "HOME") ".cache" "goblins" "tor"))

(define default-tor-socks-path
  (build-path default-goblins-tor-dir "tor-socks-sock"))

(define default-tor-control-path
  (build-path default-goblins-tor-dir "tor-control-sock"))

(define default-tor-ocapn-socks-dir
  (build-path default-goblins-tor-dir "ocapn-sockets"))

(define (expect-250-ok line)
  (match line
    ["250 OK" 'ok]
    [something-else
     (error 'wrong-response "Failed to authenticate, got: ~a" something-else)]))

(define (setup-ocapn-io tor-control-path tor-ocapn-socks-dir)
  ;; Set up the temporary directory and paths we'll be using for this
  ;; captp process
  (unless (file-exists? tor-ocapn-socks-dir)
    ;; TODO: Make this recursive?
    (mkdir tor-ocapn-socks-dir))
  (define ocapn-sock-path
    (random-tmp-filename tor-ocapn-socks-dir
                         #:format-name
                         (lambda (name)
                           ;; store the uid in the ocapn sock directory so maybe
                           ;; we could add a gc routine for obviously-unused
                           ;; old sock files
                           (format #f "ocapn-~a-~a.sock"
                                   (getuid) name))))

  (define tor-control
    (spawn-tor-control-connect-unix tor-control-path))

  (<-np tor-control 'write-line "AUTHENTICATE")
  (define ok-tor-control
    (on (<- tor-control 'read-line)
        (lambda (line)
          (expect-250-ok line)
          tor-control)
        #:promise? #t))

  (define ocapn-sock-listener-port
    (make-server-unix-domain-socket ocapn-sock-path))
  (define ocapn-sock-listener
    (spawn ^io ocapn-sock-listener-port
           #:cleanup
           (lambda (port)
             (close-port port)
             (delete-file ocapn-sock-path))))
  (values ocapn-sock-path ocapn-sock-listener ok-tor-control))

(define (new-tor-connection tor-control-path tor-ocapn-socks-dir)
  (define-values (ocapn-sock-path ocapn-sock-listener tor-control)
    (setup-ocapn-io tor-control-path tor-ocapn-socks-dir))

  (<-np tor-control 'write-line
        (format #f "ADD_ONION NEW:ED25519-V3 PORT=9045,unix:~a"
                ocapn-sock-path))

  (define service-id-vow
    (on (<- tor-control 'read-line)
        (lambda (response)
          (match (string-match "^250-ServiceID=(.+)$" response)
            [(? regexp-match? rxm)
             (regexp-substitute #f rxm 1)]
            [#f (error 'wrong-response "Expected ServiceId response, got:"
                       response)]))
        #:promise? #t))
  
  (define private-key-vow
    (on (<- tor-control 'read-line)
        (lambda (response)
          (match (string-match "^250-PrivateKey=(.+)$" response)
            [(? regexp-match? rxm)
             (regexp-substitute #f rxm 1)]
            [#f (error 'wrong-response "Expected PrivateKey response, got:"
                       response)]))
        #:promise? #t))

  (define ok-tor-control
    (on (all-of service-id-vow private-key-vow)
        (lambda _
          tor-control)
        #:catch
        (lambda (err)
          ($ tor-control 'halt)
          ($ ocapn-sock-listener 'halt))
        #:promise? #t))

  (define ok-ocapn-sock-listener
    (on (<- tor-control 'read-line)
        (lambda (line)
          (expect-250-ok line)
          ocapn-sock-listener)
        #:catch
        (lambda (err)
          ($ ocapn-sock-listener 'halt))
        #:promise? #t))

  (values ocapn-sock-path ok-ocapn-sock-listener service-id-vow private-key-vow))

(define (restore-tor-connection tor-control-path tor-ocapn-socks-dir
                                private-key service-id)
  (define-values (ocapn-sock-path ocapn-sock-listener tor-control)
    (setup-ocapn-io tor-control-path tor-ocapn-socks-dir))

  (define-values (private-key-vow private-key-resolver)
    (spawn-promise-and-resolver))
  ($ private-key-resolver 'fulfill private-key)

  (<-np tor-control 'write-line
        (format #f "ADD_ONION ~a PORT=9045,unix:~a"
                private-key
                ocapn-sock-path))
  
  (define returned-service-id-vow
    (on (<- tor-control 'read-line)
        (lambda (response)
          (match (string-match "^250-ServiceID=(.+)$" response)
            [(? regexp-match? rxm)
             (regexp-substitute #f rxm 1)]
            [#f (error 'wrong-response "Expected ServiceId response, got:"
                       response)]))
        #:promise? #t))
  (on returned-service-id-vow
      (lambda (returned-service-id)
        (unless (equal? service-id returned-service-id)
          (error "Got the wrong service-id; expected ~a but got ~a"
                 service-id returned-service-id))))

  (on (<- tor-control 'read-line) expect-250-ok)
  (values ocapn-sock-path ocapn-sock-listener))

(define-actor (^onion-netlayer-SETUP bcom private-key service-id
                                     tor-control-path
                                     tor-socks-path
                                     tor-ocapn-socks-dir
                                     #:optional
                                     init-ocapn-sock-path
                                     init-ocapn-sock-listener)
  #:portrait
  (lambda ()
    ;; Specifically do *NOT* persist the optional ocapn-socks-path or
    ;; ocapn-socks-listener, if we're being rehydrated, make them anew.
    (list private-key service-id
          tor-control-path tor-socks-path tor-ocapn-socks-dir))

  (define our-location
    (make-ocapn-peer 'onion service-id #f))

  (define-values (ocapn-sock-path ocapn-sock-listener)
    (if (and init-ocapn-sock-path init-ocapn-sock-listener)
        (values init-ocapn-sock-path init-ocapn-sock-listener)
        (restore-tor-connection tor-control-path tor-ocapn-socks-dir
                                private-key service-id)))

  (define (incoming-accept)
    (on (<- ocapn-sock-listener
            (lambda (port)
              (accept port O_NONBLOCK)))
        (lambda (accepted)
          (match accepted
            ((client . addr)
             (setvbuf client 'block 1024)
             client)))
        #:promise? #t))

  (define (outgoing-connect-location location)
    (unless (eq? (ocapn-peer-transport location) 'onion)
      (error "Wrong netlayer! Expected onion" location))
    (let* ((designator (ocapn-peer-designator location))
           (sock (make-client-unix-domain-socket tor-socks-path)))
      (spawn-fibrous-vow
       (lambda ()
         (onion-socks5-setup! sock (string-append designator ".onion") 9045)
         sock))))

  (^base-port-netlayer bcom our-location
                       incoming-accept outgoing-connect-location))

(define-actor (^onion-netlayer-FRESH bcom swap-to
                                     tor-control-path
                                     tor-socks-path
                                     tor-ocapn-socks-dir)

  ;; We're not fully setup yet, messages sent to us will go to this
  ;; vows and we'll resolve it when we're ready to handle them.
  (define-values (setup-netlayer-vow setup-netlayer-resolver)
    (spawn-promise-and-resolver))

  (define-values (ocapn-sock-path ocapn-sock-listener service-id-vow private-key-vow)
    (new-tor-connection tor-control-path tor-ocapn-socks-dir))

  (let-on ([service-id service-id-vow]
           [private-key private-key-vow])
    (let ((ready (spawn ^onion-netlayer-SETUP
                        private-key service-id
                        tor-control-path tor-socks-path tor-ocapn-socks-dir
                        ocapn-sock-path ocapn-sock-listener)))
      (<-np swap-to ready)
      (<- setup-netlayer-resolver 'fulfill ready)))
     
  (lambda args
    (match args
      [('netlayer-name) 'onion]
      [args (apply <- setup-netlayer-vow args)])))

(define* (^onion-netlayer _bcom
                          #:optional private-key service-id
                          #:key
                          [tor-control-path default-tor-control-path]
                          [tor-socks-path default-tor-socks-path]
                          [tor-ocapn-socks-dir default-tor-ocapn-socks-dir])
  "Constructs the onion netlayer to enable communicate over Tor's Onion Services

It can be spawned with no arguments to create a fresh netlayer, or
provided with:

- PRIVATE-KEY the private key for a onion service
- SERVICE-ID the service ID to use for the onion service

If reconstructing the onion netlayer, both PRIVATE-KEY and SERVICE-ID
must be provided."
  (define-values (swap-to-vow swap-to-resolver)
    (spawn-promise-and-resolver))
  
  (define netlayer
    (if (and private-key service-id)
        (spawn ^onion-netlayer-SETUP
               private-key service-id
               tor-control-path tor-socks-path tor-ocapn-socks-dir)
        (spawn ^onion-netlayer-FRESH swap-to-vow
               tor-control-path tor-socks-path tor-ocapn-socks-dir)))

  (define-values (proxy swap-to)
    (swappable netlayer '^onion-netlayer))
  ($ swap-to-resolver 'fulfill swap-to)
  proxy)

(define onion-netlayer-env
  (make-persistence-env
   `((((goblins ocapn netlayer onion) ^onion-netlayer-FRESH) ,^onion-netlayer-FRESH)
     (((goblins ocapn netlayer onion) ^onion-netlayer-SETUP) ,^onion-netlayer-SETUP))
   #:extends swappable-env))
