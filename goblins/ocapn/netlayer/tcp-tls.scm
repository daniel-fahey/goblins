;;; Copyright 2023 David Thompson
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

(define-module (goblins ocapn netlayer tcp-tls)
  #:use-module (gcrypt base16)
  #:use-module ((gcrypt hash) #:prefix gcrypt:)
  #:use-module (gnutls)
  #:use-module (goblins)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer base-port)
  #:use-module (goblins actor-lib io)
  #:use-module (goblins utils crypto)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (rnrs bytevectors)
  #:export (generate-tls-private-key
            generate-tls-certificate
            ^tcp-tls-netlayer
            tcp-tls-netlayer-env))

(define (use-nonblocking-i/o port)
  (fcntl port F_SETFL (logior O_NONBLOCK (fcntl port F_GETFL))))

(define (make-server-socket+port port max-connections)
  (let ((sock (socket AF_INET SOCK_STREAM IPPROTO_TCP)))
    (bind sock AF_INET INADDR_ANY (or port 0))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (fcntl sock F_SETFD FD_CLOEXEC)
    (use-nonblocking-i/o sock)
    (listen sock max-connections)
    (values sock (vector-ref (getsockname sock) 2))))

(define (make-client-socket host port)
  ;; Resolve hostname to get IP address.
  (match (getaddrinfo host (number->string port)
                      AI_NUMERICSERV AF_INET SOCK_STREAM IPPROTO_TCP)
    ((info _ ...)
     (let* ((family (addrinfo:fam info))
            (socktype (addrinfo:socktype info))
            (address (sockaddr:addr (addrinfo:addr info)))
            (sock (socket family socktype IPPROTO_TCP)))
       (use-nonblocking-i/o sock)
       (connect sock family address port)
       sock))))

;; The GnuTLS bindings used for generating keys/certs are only in
;; bleeding edge versions of guile-gnutls, so we cannot rely on them
;; being there.
(define (gnutls-procedure-exists? name)
  (variable? (module-variable (resolve-interface '(gnutls)) name)))

(define (%generate-tls-private-key)
  ;; For compatibility with older Guile-GnuTLS versions that do not
  ;; support generation and exporting, we have to hand back a
  ;; bytevector that can be imported into a TLS session.
  (let ((key (generate-x509-private-key pk-algorithm/rsa 4096 '())))
    (export-x509-private-key key x509-certificate-format/pem)))

(define (generate-tls-private-key)
  "Return a freshly generated X.509 private key."
  (if (gnutls-procedure-exists? 'generate-x509-private-key)
      (%generate-tls-private-key)
      (throw 'missing-gnutls-binding 'generate-x509-private-key)))

(define (%generate-tls-certificate key)
  (let* ((cert (make-x509-certificate))
         ;; We're stuck with bytevectors for holding key data so that
         ;; we stay compatible with older versions of Guile-GnuTLS,
         ;; hence this silly re-import here.
         (key* (import-x509-private-key key x509-certificate-format/pem))
         (activation-time (current-time))
         ;; The maximum possible expiration time, at least the maximum
         ;; allowed by GnuTLS, is the end of the UNIX epoch in 2038.
         ;; Not great!
         (expiration-time 2147483647))
    (set-x509-certificate-version! cert 3)
    (set-x509-certificate-activation-time! cert activation-time)
    (set-x509-certificate-expiration-time! cert expiration-time)
    ;; A lot of this data is just boilerplate so that all the
    ;; necessary fields are populated for the certificate to be valid.
    (set-x509-certificate-dn-by-oid! cert oid/x520-country-name "US")
    (set-x509-certificate-dn-by-oid! cert oid/x520-state-or-province-name "DE")
    (set-x509-certificate-dn-by-oid! cert oid/x520-locality-name "Lewes")
    (set-x509-certificate-dn-by-oid! cert oid/x520-common-name "CapTP")
    (set-x509-certificate-dn-by-oid! cert oid/x520-organization-name "Spritely")
    (set-x509-certificate-dn-by-oid! cert oid/x520-organizational-unit-name
                                     "Goblins")
    ;; We aren't using CAs.
    (set-x509-certificate-ca-status! cert #f)
    ;; Likewise, the serial number is really only important for CAs.
    ;; The cert has to have one, though, so we hardcode a silly
    ;; message that conforms to the conventional 20 byte length.
    (set-x509-certificate-serial! cert (string->utf8 "OCapN cereal number!"))
    ;; Add the private key and self-sign the cert.
    (set-x509-certificate-key! cert key*)
    (set-x509-certificate-key-usage! cert (list key-usage/digital-signature
                                                key-usage/key-encipherment))
    (set-x509-certificate-subject-key-id! cert (x509-certificate-key-id cert))
    (sign-x509-certificate! cert cert key*)
    ;; For compatibility with older Guile-GnuTLS versions that do not
    ;; support generation and exporting, we have to hand back a
    ;; bytevector that can be imported into a TLS session.
    (export-x509-certificate cert x509-certificate-format/pem)))

(define (generate-tls-certificate key)
  "Return a freshly generated X.509 certificate for KEY."
  (if (gnutls-procedure-exists? 'make-x509-certificate)
      (%generate-tls-certificate key)
      (throw 'missing-gnutls-binding 'make-x509-certificate)))

(define (tls-retry proc tls-session)
  (define (handle-error exception)
    (match (exception-args exception)
      (((? fatal-error?) . _)
       (raise-exception exception))
      ;; For non-fatal errors such as EAGAIN or EINTERRUPTED, give
      ;; control to the current read waiter and try again when it
      ;; returns.
      (_
       ((current-read-waiter) (session-record-port tls-session))
       (tls-retry proc tls-session))))
  (with-exception-handler handle-error
    (lambda () (proc tls-session))
    #:unwind? #t
    #:unwind-for-type 'gnutls-error))

(define (tls-handshake tls-session)
  (tls-retry handshake tls-session))

(define (tls-validate-peer-certificate tls-session)
  (match (peer-certificate-status tls-session)
    (() #t) ; valid cert
    ((issues ...)
     (throw 'invalid-tls-certificate issues))))

(define (make-server-tls-session cert key)
  (let ((tls-session (make-session connection-end/server))
        (creds (make-certificate-credentials)))
    (set-session-default-priority! tls-session)
    (set-certificate-credentials-x509-key-data! creds cert key
                                                x509-certificate-format/pem)
    (set-session-credentials! tls-session creds)
    tls-session))

(define (make-client-tls-session)
  (let ((tls-session (make-session connection-end/client)))
    (set-session-default-priority! tls-session)
    tls-session))

(define (make-tls-port tls-session port)
  (let ((tls-port (session-record-port tls-session)))
    (define (close _)
      (close-port port))
    (set-session-transport-fd! tls-session (fileno port))
    (set-session-record-port-close! tls-port close)
    ;; Use block buffering, not line buffering, since we're dealing with
    ;; encrypted binary data and not lines of text.
    (setvbuf port 'block)
    tls-port))

(define (make-server-tls-port port cert key)
  (let* ((tls-session (make-server-tls-session cert key))
         (tls-port (make-tls-port tls-session port)))
    ;; Before the TLS handshake is performed, the server needs to send
    ;; their cert to the client so they can verify that it hashes to
    ;; the value they expect.  The server writes a 32-bit unsigned
    ;; integer containing the length of the cert, and then the cert
    ;; itself.
    (put-bytevector port (u32vector (bytevector-length cert)))
    (put-bytevector port cert)
    (force-output port)
    (tls-handshake tls-session)
    tls-port))

(define* (make-client-tls-port port cert key trust-hash)
  (let* ((tls-session (make-client-tls-session))
         (tls-port (make-tls-port tls-session port))
         ;; Before the TLS handshake is performed, the client needs to
         ;; fetch the cert from the server and verify that its hash matches
         ;; the expected hash given in the node location.
         (cert-length (u32vector-ref (get-bytevector-n port 4) 0))
         (server-cert (get-bytevector-n port cert-length))
         (server-cert-hash (sha256d server-cert))
         (creds (make-certificate-credentials)))
    ;; Load our own credentials.
    (set-certificate-credentials-x509-key-data! creds cert key
                                                x509-certificate-format/pem)
    ;; Bail if the hash doesn't match what we expect.
    (unless (equal? trust-hash server-cert-hash)
      (throw 'tls-cert-hash-mismatch
             (bytevector->base16-string trust-hash)
             (bytevector->base16-string server-cert-hash)))
    (set-certificate-credentials-x509-trust-data! creds server-cert
                                                  x509-certificate-format/pem)
    (set-session-credentials! tls-session creds)
    (tls-handshake tls-session)
    ;; The cert that the server sent matched the hash, but is it a
    ;; valid cert?  Let's make sure!
    (tls-validate-peer-certificate tls-session)
    tls-port))

(define (ocapn-node-hint:host node)
  (match (assq-ref (ocapn-node-hints node) 'host)
    (() #f)
    ((host) host)))

(define (ocapn-node-hint:port node)
  (or (match (assq-ref (ocapn-node-hints node) 'port)
        (() #f)
        ((port)
         (string->number port)))
      8088))

(define-actor (^tcp-tls-netlayer bcom host #:key port
                                 [max-connections 32]
                                 [key (generate-tls-private-key)]
                                 [cert (generate-tls-certificate key)])
    "Spawn and return a new TCP + TLS netlayer.  HOST specifies the
hostname that appears in the OCapN sturdyrefs that use this netlayer.

If PORT is specified, the netlayer will listen for incoming
connections on that port or throw an error if the port is already in
use.  If PORT is not specified, an open port will be chosen
automatically.

MAX-CONNECTIONS specifies the number of peers that may be connected to
the netlayer at any given time.

KEY and CERT specify the X.509 private key and certificate to use for
encrypting connections.  If one or both are unspecified, they will be
automatically generated provided that the version of Guile-GnuTLS is
new enough to do so.  To import PEM encoded private keys and
certificates from the file system, use 'load-tls-private-key' and
'load-tls-certificate', respectively.  Automatically generated keys
and certificates are useful for nodes that do not need persistent
identity across process lifetimes, but nodes that do should import
from the file system."
  (define-values (server-socket server-port)
    (make-server-socket+port port max-connections))
  (define server-socket-io
    (spawn ^io server-socket))
  (define our-location
    (make-ocapn-node 'tcp-tls
                     (bytevector->base16-string (sha256d cert))
                     `((host ,host)
                       (port ,(number->string server-port)))))
  (define (incoming-accept)
    (on (<- server-socket-io (lambda (resource) (accept resource)))
        (match-lambda
          ((client-socket . _)
           (spawn ^read-write-io client-socket
                  #:init
                  (lambda (sock)
                    (setvbuf sock 'block)
                    (use-nonblocking-i/o sock)
                    (make-server-tls-port sock cert key)))))
        #:promise? #t))
  (define (outgoing-connect-location location)
    (unless (eq? (ocapn-node-transport location) 'tcp-tls)
      (error "Wrong netlayer! Expected `tcp-tls'" location))
    (let* ((host (ocapn-node-hint:host location))
           (port (ocapn-node-hint:port location))
           (server-cert-hash (base16-string->bytevector
                              (ocapn-node-designator location)))
           (client-socket (make-client-socket host port)))
      (spawn ^read-write-io
             #:init
             (lambda (sock)
               (make-client-tls-port sock cert key server-cert-hash))
             #:cleanup
             (lambda (sock)
               (close-port sock)))))
  (^base-port-netlayer bcom our-location incoming-accept
                       outgoing-connect-location))

(define tcp-tls-netlayer-env
  (make-persistence-env
   `((((goblins ocapn netlayer tcp-tls) ^tcp-tls-netlayer) ,^tcp-tls-netlayer))))
