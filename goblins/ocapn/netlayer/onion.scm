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
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib io)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer utils)
  #:use-module (goblins ocapn netlayer onion-socks)
  #:use-module (goblins ocapn netlayer base-port)
  #:use-module (goblins contrib syrup)
  #:export (new-onion-netlayer
            restore-onion-netlayer
            ^onion-netlayer
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

  ($ tor-control 'write-line "AUTHENTICATE")
  (on (<- tor-control 'read-line) expect-250-ok)

  (define ocapn-sock-listener-port
    (make-server-unix-domain-socket ocapn-sock-path))
  (define ocapn-sock-listener
    (spawn ^io ocapn-sock-listener-port
           #:cleanup
           (lambda (port)
             (close-port port))))
  (values ocapn-sock-path ocapn-sock-listener tor-control))

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

  (on (<- tor-control 'read-line) expect-250-ok)
  (values ocapn-sock-path ocapn-sock-listener service-id-vow private-key-vow))

(define (restore-tor-connection tor-control-path tor-ocapn-socks-dir
                                private-key service-id)
  (define-values (ocapn-sock-path ocapn-sock-listener tor-control)
    (setup-ocapn-io tor-control-path tor-ocapn-socks-dir))

  (define-values (private-key-vow private-key-resolver)
    (spawn-promise-values))
  ($ private-key-resolver 'fulfill private-key)

  ($ tor-control 'write-line
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
  (values ocapn-sock-path ocapn-sock-listener
          returned-service-id-vow private-key-vow))

(define-actor (^onion-netlayer* bcom
                                self
                                private-key service-id
                                tor-control-path
                                tor-socks-path
                                tor-ocapn-socks-dir)
  (define (setup-onion-netlayer private-key service-id our-location
                                ocapn-sock-path ocapn-sock-listener)
    (define (incoming-accept)
      (on (<- ocapn-sock-listener
              (lambda (port)
                (accept port SOCK_NONBLOCK)))
          (lambda (accepted)
            (match accepted
              ((client . addr)
               (setvbuf client 'block 1024)
               client)))
          #:promise? #t))
    (define (outgoing-connect-location location)
      (unless (eq? (ocapn-node-transport location) 'onion)
        (error "Wrong netlayer! Expected onion" location))
      (let* ((designator (ocapn-node-designator location))
             (sock (make-client-unix-domain-socket tor-socks-path)))
        (onion-socks5-setup! sock (string-append designator ".onion")
                             9045)
        sock))
    (^base-port-netlayer bcom our-location
                         incoming-accept outgoing-connect-location))

  ;; We have to wait until the tor daemon (or aurie) gives us the
  ;; information we need to fully be setup. We use two things to do
  ;; that:
  ;; 1. the `setup-beh' cell which is filled with the behavior from
  ;;    the ^base-port-netlayer once we're setup. The next time we're
  ;;    sent a message we'll bcom that behavior.
  ;;
  ;; 2. While we're still not setup, we might be sent messages, those
  ;;    are sent to the setup-netlayer-vow. Once we're setup we
  ;;    resolve the promise to ourselves which will forward all
  ;;    messages to us when we're able to process them.
  (define setup-beh (spawn ^cell))
  (define-values (setup-netlayer-vow setup-netlayer-resolver)
    (spawn-promise-values))

  ;; When we have the values for the private-key and service-id we can see
  ;; about setting ourselves up, either by restoring with the keys provided
  ;; or setting up a new tor connection.
  (define tor-connection-vow
    (on (all-of private-key service-id)
        (match-lambda
          [(private-key* service-id*)
           (define-values (ocapn-sock-path ocapn-sock-listener service-id-vow private-key-vow)
             (if (and ($ private-key*) ($ service-id*))
                 (restore-tor-connection tor-control-path tor-ocapn-socks-dir
                                         ($ private-key*) ($ service-id*))
                 (new-tor-connection tor-control-path tor-ocapn-socks-dir)))

           (list ocapn-sock-path ocapn-sock-listener service-id-vow private-key-vow)])
        #:promise? #t))

  (on tor-connection-vow
      (match-lambda
        [(ocapn-sock-path ocapn-sock-listener service-id-vow private-key-vow)
         ;; Save the private key and service ID back in the cells
         (on (all-of private-key-vow service-id-vow)
             (lambda (private-key-and-service-id)
               (define-values (private-key* service-id*)
                 (match private-key-and-service-id
                   [(private-key* service-id*)
                    (values private-key* service-id*)]))
               ;; Add the info we need to restore to the cells so aurie can store it
               ($ service-id service-id*)
               ($ private-key private-key*)
               ;; Finally switch to the "ready" beh
               (let ((base-port-beh
                      (setup-onion-netlayer private-key*
                                            service-id*
                                            (make-ocapn-node 'onion service-id* #f)
                                            ocapn-sock-path
                                            ocapn-sock-listener)))
                 ($ setup-beh base-port-beh)
                 ($ setup-netlayer-resolver 'fulfill ($ self)))))]))


  (match-lambda
    [('netlayer-name) 'onion]
    [args
     (let ((setup-beh ($ setup-beh)))
       (if setup-beh
           (bcom setup-beh (apply setup-beh args))
           (apply <- setup-netlayer-vow args)))]))

(define* (^onion-netlayer _bcom
                          #:optional private-key service-id
                          #:key
                          [tor-control-path default-tor-control-path]
                          [tor-socks-path default-tor-socks-path]
                          [tor-ocapn-socks-dir default-tor-ocapn-socks-dir])
  (define self (spawn ^cell))
  (define netlayer
    (spawn ^onion-netlayer*
           self
           (spawn ^cell private-key)
           (spawn ^cell service-id)
           tor-control-path
           tor-socks-path
           tor-ocapn-socks-dir))
  ($ self netlayer)
  netlayer)

(define onion-netlayer-env
  (make-persistence-env
   `((((goblins ocapn netlayer onion) ^onion-netlayer) ,^onion-netlayer*))
   #:extends cell-env))
