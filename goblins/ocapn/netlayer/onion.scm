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
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins ocapn structs-urls)
  #:use-module (goblins ocapn netlayer utils)
  #:use-module (goblins ocapn netlayer onion-socks)
  #:export (new-onion-netlayer
            restore-onion-netlayer))

(define default-goblins-port 9045)
(define default-goblins-tor-dir
  (string-append (getenv "HOME") "/.cache/goblins/tor/"))
(define default-tor-socks-path
  (string-append default-goblins-tor-dir "tor-socks-sock"))
(define default-tor-control-path
  (string-append default-goblins-tor-dir "tor-control-sock"))
(define default-tor-ocapn-socks-dir
  (string-append default-goblins-tor-dir "ocapn-sockets"))

(define* (restore-onion-netlayer
          service-id private-key
          #:key
          (tor-control-path default-tor-control-path)
          (tor-socks-path default-tor-socks-path)
          (tor-ocapn-socks-dir default-tor-ocapn-socks-dir))
  (define-values (tor-sock
                  ocapn-tmp-dir ocapn-sock-path ocapn-sock-listener)
    (restore-tor-connection tor-control-path tor-ocapn-socks-dir
                            service-id private-key))

  (finish-onion-setup service-id private-key tor-sock
                      tor-socks-path ocapn-tmp-dir ocapn-sock-path
                      ocapn-sock-listener))

(define* (new-onion-netlayer
          #:key
          (tor-control-path default-tor-control-path)
          (tor-socks-path default-tor-socks-path)
          (tor-ocapn-socks-dir default-tor-ocapn-socks-dir))
  (define-values (tor-sock
                  ocapn-tmp-dir ocapn-sock-path ocapn-sock-listener
                  service-id-vow private-key-vow)
    (new-tor-connection tor-control-path tor-ocapn-socks-dir))

  (finish-onion-setup service-id-vow private-key-vow tor-sock
                      tor-socks-path ocapn-tmp-dir ocapn-sock-path
                      ocapn-sock-listener))

(define (finish-onion-setup service-id-vow private-key-vow tor-sock
                            tor-sock-path ocapn-temp-dir ocapn-sock-path
                            ocapn-sock-listener)
  (define (do-cleanup)
    (delete-file ocapn-sock-path)
    (delete-file ocapn-temp-dir)
    (<- tor-sock 'shutdown))

  (define our-location-vow
    (on service-id-vow
        (lambda (service-id)
          (make-ocapn-machine 'onion service-id #f))
        #:promise? #t))

  (values (spawn ^onion-netlayer our-location-vow ocapn-sock-listener
                 tor-sock-path do-cleanup)
          service-id-vow private-key-vow))

(define (expect-250-ok msg)
  (match msg
    ("250 OK" #t)
    (something-else
     (error (format #f "Failed to authenticate, got ~a" something-else)))))

(define (read-service-id msg)
  (define matched
    (string-match "^250-ServiceID=(.+)$" msg))
  (if matched
      (match:substring matched 1)
      (error (format #f "Expected ServiceId response, got ~a" msg))))

(define (read-private-key msg)
  (define matched
    (string-match "^250-PrivateKey=(.+)$" msg))
  (if matched
      (match:substring matched 1)
      (error (format #f "Expected PrivateKey response, got ~a" msg))))

(define (setup-ocapn-io tor-control-path tor-ocapn-socks-dir)
  (unless (file-exists? tor-ocapn-socks-dir)
    (mkdir tor-ocapn-socks-dir))
  (define ocapn-socks-dir
    (mkdtemp (format #f "~a/XXXXXX" tor-ocapn-socks-dir)))
  (define ocapn-socks-path
    (string-append ocapn-socks-dir "ocapn.sock"))

  (define tor-sock
    (spawn ^unix-socket tor-control-path))

  (on (<- tor-sock 'ask "AUTHENTICATE\r\n")
      (lambda (msg)
        (pk 'msg msg)))

  (values ocapn-socks-dir ocapn-socks-path
          #f tor-sock))

(define (new-tor-connection tor-control-path tor-ocapn-socks-dir)
  (define-values (ocapn-sock-dir
                  ocapn-sock-path ocapn-sock-listener
                  tor-sock)
    (setup-ocapn-io tor-control-path tor-ocapn-socks-dir))

  (define add-onion-message
    (format #f "ADD_ONION NEW:ED25519-V3 PORT=~a,unix:~a\r\n"
            default-goblins-port ocapn-sock-path))

  (define service-id-and-private-key-vow
    (on (<- tor-sock 'ask add-onion-message #:replies 3)
        (lambda (replies)
          (when (expect-250-ok (list-ref replies 2))
            (cons (read-service-id (list-ref replies 0))
                  (read-private-key (list-ref replies 1)))))
        #:promise? #t))

  (define service-id-vow
    (on service-id-and-private-key-vow
        (lambda (service-id-and-private-key)
          (car service-id-and-private-key))
        #:promise? #t))
  (define private-key-vow
    (on service-id-and-private-key-vow
        (lambda (service-id-and-private-key)
          (cdr service-id-and-private-key))
        #:promise? #t))

  (values tor-sock
          ocapn-sock-dir ocapn-sock-path ocapn-sock-listener
          service-id-vow private-key-vow))

(define (restore-tor-connection tor-control-path tor-ocapn-socks-dir service-id private-key)
  (define-values (ocapn-sock-dir
                  ocapn-sock-path ocapn-sock-listener
                  tor-sock)
    (setup-ocapn-io tor-control-path tor-ocapn-socks-dir))

  (define add-onion-message
    (format #f "ADD_ONION ~a PORT=~a,unix:~a\r\n"
            private-key default-goblins-port
            ocapn-sock-path))

  (on (<- tor-sock 'ask add-onion-message #:replies 2)
      (lambda (replies)
        (let ((returned-service-id (read-service-id (car replies))))
          (unless (and (expect-250-ok (list-ref replies 1))
                       (string=? returned-service-id service-id))
            (error (format #f "Got the wrong service-id; expected ~a but got ~a"
                           service-id returned-service-id))))))

  (values tor-sock ocapn-sock-dir ocapn-sock-path
          ocapn-sock-listener))

(define (^onion-netlayer bcom our-location-vow
                         ocapn-socks-listener tor-sock-path
                         do-cleanup)
  (define (start-listen conn-establisher)
    'todo)

  (define base-beh
    (methods
     [(netlayer-name) 'onion]
     [(our-location) our-location-vow]))

  (define pre-setup-beh
    (extend-methods
     base-beh
     [(setup conn-establisher) 'TODO]))

  (define (ready-beh conn-establisher)
    (define (^start-connection _bcom address)
      (lambda ()
        (define sock
          (spawn ^unix-socket tor-sock-path))
        ;; This currently expects to work on a raw socket, not a wrapped
        ;; ^unix-socket, it probably wants re-writing to work with it.
        (onion-socks5-setup! sock
                             (string-append address ".onion")
                             default-goblins-port)

        ;; TODO: need to wrap the channels with syrup serialization.
        (<- conn-establisher send-ch recieve-ch #f)))
    (extend-methods
     base-beh
     [(self-location? loc) #t] ;; TODO: this is implemented on the fake-netlayer branch.
     [(connect-to remote-machine)
      (match remote-machine
        (($ <ocapn-machine> 'onion address #f)
         (let ((connect-vat (spawn-vat)))
           (<- (connect-vat 'spawn ^start-connection address)))))]))
  pre-setup-beh)
