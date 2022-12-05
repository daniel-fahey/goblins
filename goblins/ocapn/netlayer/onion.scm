;;; Copyright 2021-2022 Christine Lemmer-Webber
;;; Copyright 2022 Jessica Tallon
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
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (fibers channels)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer utils)
  #:use-module (goblins ocapn netlayer onion-socks)
  #:use-module (goblins ocapn netlayer base-port)
  #:use-module (goblins contrib syrup)
  #:export (new-onion-netlayer
            restore-onion-netlayer))

(define (tor-control-connect-unix path)
  (define sock
    (make-client-unix-domain-socket path))
  (line-delimited-ports->channels sock sock))

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

(define (expect-250-ok tor-in-ch)
  (match (get-message tor-in-ch)
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

  (define-values (tor-in-ch tor-out-ch)
    (tor-control-connect-unix tor-control-path))
  
  (put-message tor-out-ch "AUTHENTICATE")
  (expect-250-ok tor-in-ch)

  (define ocapn-sock-listener (make-server-unix-domain-socket ocapn-sock-path))
  (values ocapn-sock-path ocapn-sock-listener
          tor-in-ch tor-out-ch))

(define (new-tor-connection tor-control-path tor-ocapn-socks-dir)
  (define-values (ocapn-sock-path ocapn-sock-listener
                  tor-in-ch tor-out-ch)
    (setup-ocapn-io tor-control-path tor-ocapn-socks-dir))

  (put-message tor-out-ch
               (format #f "ADD_ONION NEW:ED25519-V3 PORT=9045,unix:~a"
                       ocapn-sock-path))

  (define service-id
    (let ([response (get-message tor-in-ch)])
      (match (string-match "^250-ServiceID=(.+)$" response)
        [(? regexp-match? rxm)
         (regexp-substitute #f rxm 1)]
        [#f (error 'wrong-response "Expected ServiceId response, got:"
                   response)])))
  (define private-key
    (let ([response (get-message tor-in-ch)])
      (match (string-match "^250-PrivateKey=(.+)$" response)
        [(? regexp-match? rxm)
         (regexp-substitute #f rxm 1)]
        [#f (error 'wrong-response "Expected PrivateKey response, got:"
                   response)])))

  (expect-250-ok tor-in-ch)
  
  (values ocapn-sock-path ocapn-sock-listener service-id private-key))

(define (restore-tor-connection tor-control-path tor-ocapn-socks-dir
                                private-key service-id)
  (define-values (ocapn-sock-path ocapn-sock-listener
                  tor-in-ch tor-out-ch)
    (setup-ocapn-io tor-control-path tor-ocapn-socks-dir))
  (put-message tor-out-ch
               (format #f "ADD_ONION ~a PORT=9045,unix:~a"
                       private-key
                       ocapn-sock-path))
  (define returned-service-id
    (let ([response (get-message tor-in-ch)])
      (match (string-match "^250-ServiceID=(.+)$" response)
        [(? regexp-match? rxm)
         (regexp-substitute #f rxm 1)]
        [#f (error 'wrong-response "Expected ServiceId response, got:"
                   response)])))
  (unless (equal? service-id returned-service-id)
    (error "Got the wrong service-id; expected ~a but got ~a"
           service-id returned-service-id))
  (expect-250-ok tor-in-ch)

  (values ocapn-sock-path ocapn-sock-listener))

(define (^onion-netlayer bcom our-location ocapn-sock-listener
                         tor-socks-path do-cleanup
                         private-key service-id)
  (define (incoming-accept)
    (match (accept ocapn-sock-listener SOCK_NONBLOCK)
      ((client . addr)
       (setvbuf client 'block 1024)
       ;; (As said in the Fibers manual:)
       ;; Disable Nagle's algorithm.  We buffer ourselves.
       (setsockopt client IPPROTO_TCP TCP_NODELAY 1)
       client)))
  (define (outgoing-connect-location location)
    (unless (eq? (ocapn-machine-transport location) 'onion)
      (error "Wrong netlayer! Expected onion" location))
    (let* ((address (ocapn-machine-address location)))
      (define sock (make-client-unix-domain-socket tor-socks-path))
      (onion-socks5-setup! sock (string-append address ".onion")
                           9045)
      sock))
  (^base-port-netlayer bcom our-location
                       incoming-accept outgoing-connect-location))

(define (_finish-setup-onion private-key service-id tor-socks-path
                             ocapn-sock-path ocapn-sock-listener)
  ;; TODO: Cleanup tor subprocess also.
  (define (do-cleanup)
    (close-port ocapn-sock-listener)
    (delete-file ocapn-sock-path))

  (define our-location
    (make-ocapn-machine 'onion service-id #f))

  (spawn ^onion-netlayer our-location ocapn-sock-listener
         tor-socks-path do-cleanup
         private-key service-id))

;; TODO: I guess this really *should* return one value to its
;; continuation, and the pre-setup-beh above should also support
;; our-location
(define* (new-onion-netlayer
          #:key
          [tor-control-path default-tor-control-path]
          [tor-socks-path default-tor-socks-path]
          [tor-ocapn-socks-dir default-tor-ocapn-socks-dir])
  (define-values (ocapn-sock-path ocapn-sock-listener service-id private-key)
    (new-tor-connection tor-control-path tor-ocapn-socks-dir))

  (_finish-setup-onion private-key service-id tor-socks-path
                       ocapn-sock-path ocapn-sock-listener))

(define* (restore-onion-netlayer
          service-id private-key
          #:key [tor-control-path default-tor-control-path]
          [tor-socks-path default-tor-socks-path]
          [tor-ocapn-socks-dir default-tor-ocapn-socks-dir])
  (define-values (ocapn-sock-path ocapn-sock-listener)
    (restore-tor-connection tor-control-path tor-ocapn-socks-dir
                            private-key service-id))
  (_finish-setup-onion private-key service-id tor-socks-path
                       ocapn-sock-path ocapn-sock-listener))


