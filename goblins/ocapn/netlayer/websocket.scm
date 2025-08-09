;;; Copyright 2024 Juliana Sims
;;; Copyright 2024 Jessica Tallon
;;; Copyright 2025 David Thompson <dave@spritely.institute>
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

(define-library (goblins ocapn netlayer websocket)
  (export generate-tls-private-key
          generate-tls-certificate
          ^websocket-netlayer
          websocket-netlayer-env)
  (import (except (guile) spawn)
          (fibers)
          (fibers conditions)
          (fibers operations)
          (only (fibers timers) sleep-operation)
          (except (goblins core) $)
          (prefix (goblins core) $)
          (goblins vat)
          (goblins define-actor)
          (goblins actor-lib cell)
          (goblins actor-lib io)
          (goblins actor-lib on)
          (goblins actor-lib methods)
          (goblins actor-lib swappable)
          (goblins contrib syrup)
          (goblins ocapn ids)
          (goblins utils base32)
          (goblins utils crypto)
          (goblins utils hashmap)
          (goblins utils js-data)
          (ice-9 match)
          (except (rnrs bytevectors) bytevector-copy)
          (only (scheme base) bytevector-copy)
          (srfi srfi-11)
          (web uri))
  (cond-expand
   (guile
    (import (fibers io-wakeup)
            (goblins ocapn netlayer tcp-tls)
            (ice-9 ports internal) ; this is a hack
            (web client)
            (web request)
            (web response)
            (web socket client)
            (web socket frame)
            (web socket server)
            (web socket utils)))
   (hoot
    (import (fibers channels)
            (goblins inbox)
            (hoot ffi)
            (srfi srfi-9))))
  (begin
    (define (warn message)
      (format (current-error-port) "warning: ~a\n" message))

    (cond-expand
     (guile)
     (hoot
      (define (generate-tls-private-key)
        (error "Cannot generate TLS private key on Hoot"))
      (define (generate-tls-certificate key)
        (error "Cannot generate TLS certificate on Hoot"))))

    (define (use-nonblocking-i/o port)
      (cond-expand
       (guile
        ;; TODO: This is lifted from (goblins ocapn netlayer utils)
        ;; ideally we'd import it, but it's not compatible with Hoot
        ;; yet.
        (fcntl port F_SETFL
               (logior O_NONBLOCK (fcntl port F_GETFL))))
       ;; All I/O is non-blocking on the web and furthermore there is
       ;; no POSIX socket API.
       (hoot))
      (values))

    (define (wait-for-port port)
      (cond-expand
       (guile
        ;; KLUDGE: When 'port' is a GnuTLS port it is unusable for
        ;; 'wait-until-port-readable-operation' because it isn't a
        ;; file port.  So, we use Guile's *internal* port API to pull
        ;; out the read-wait-fd and make a file input port from that.
        ;; Bleh!
        (define (coerce-file-input-port port)
          (if (file-port? port)
              port
              (fdes->inport (port-read-wait-fd port))))
        (perform-operation
         (choice-operation
          (wrap-operation (wait-until-port-readable-operation
                           (coerce-file-input-port port))
                          (lambda () 'ready))
          (wrap-operation (sleep-operation 30)
                          (lambda () 'timeout)))))
       (hoot (values))))

    ;; Server is Guile only.
    (cond-expand
     (guile
      ;; Receives clients from the server actor.
      (define-actor (^listener bcom designator-key conn-establisher)
        (lambda (ws)
          (define (verify-designator)
            (spawn-fibrous-vow
             (lambda ()
               ;; Read a message from the client which should be a
               ;; bytevector.  Send it back signed for the client
               ;; to verify.
               (let ((bv (websocket-receive ws #:wait-for-port wait-for-port)))
                 (unless (and (bytevector? bv) (= (bytevector-length bv) 64))
                   (close-websocket ws)
                   (error "invalid client verification message"))
                 (let ((sig (sign bv (key-pair->private-key designator-key))))
                   (websocket-send ws (syrup-encode sig) #:mask? #f))
                 #t))))
          (on (verify-designator)
              (lambda (_)
                (let ((io (spawn ^websocket-captp-io ws #t)))
                  (<-np conn-establisher io #f)))
              #:catch (lambda (exn) (close-websocket ws)))))

      (define-actor (^websocket-server bcom #:key host port max-clients
                                       tls-private-key tls-certificate
                                       listener)
        (define stopped? (make-condition))
        (define-values (sock assigned-port)
          (make-server-socket host port))
        (define (make-server-socket host port)
          (let ((sock (socket PF_INET SOCK_STREAM IPPROTO_IP)))
            (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
            (use-nonblocking-i/o sock)
            (bind sock AF_INET (inet-pton AF_INET host) port)
            (values sock (vector-ref (getsockname sock) 2))))
        (define (handle-client ws)
          (<-np-extern listener ws))
        (define (accept* server-sock)
          ;; Accept might throw errors, e.g. file handle exhaustion, catch those
          ;; and return #t so we can loop again instead of throwing and taking
          ;; down the whole netlayer.
          (with-exception-handler
           (lambda (exn)
             ;; Short delay to back off before retrying the accept loop.
             (sleep 1)
             #t)
           (lambda ()
             (accept server-sock (logior O_CLOEXEC O_NONBLOCK)))
           #:unwind? #t))
        (define (accept-client server-sock)
          (perform-operation
           (choice-operation
            ;; FIXME: Use accept-operation when it's available in an
            ;; official fibers release.  This is a cheap approximation
            ;; of it to use for now.
            (wrap-operation (wait-until-port-readable-operation server-sock)
                            (lambda ()
                              ;; Since the port is readable and we are
                              ;; *only* listening for new clients on
                              ;; this port and not doing other read
                              ;; operations, the accept call shouldn't
                              ;; return #f, but if it does we will
                              ;; throw a match error.
                              (match (accept* server-sock)
                                ((ws . _)
                                 (use-nonblocking-i/o ws)
                                 ws)
                                (#t #t))))
            (wrap-operation (wait-operation stopped?) (lambda () #f)))))
        (unless (and tls-private-key tls-certificate)
          (warn "WebSocket server traffic is unencrypted"))
        (syscaller-free-fiber
         (lambda ()
           (run-websocket-server handle-client
                                 #:server-socket sock
                                 #:accept-client accept-client
                                 #:spawn-client spawn-fiber
                                 #:max-clients max-clients
                                 #:tls-private-key tls-private-key
                                 #:tls-certificate tls-certificate)
           (close-port sock)))
        (methods
         ((get-port) assigned-port)
         ((halt)
          (signal-condition! stopped?)
          ;; Spin until server socket closes.
          ;; FIXME: Do something smarter.
          (spawn-fibrous-vow
           (lambda ()
             (let lp ()
               (or (port-closed? sock)
                   (begin
                     (sleep 0.1)
                     (lp))))))))))
     (hoot))

    ;; WebSocket client abstraction for Hoot that mimicks
    ;; guile-websocket's API.
    (cond-expand
     (guile)
     (hoot
      (define-foreign %open-websocket
        "webSocket" "new"
        (ref string) -> (ref extern))
      (define-foreign %close-websocket
        "webSocket" "close"
        (ref extern) -> none)
      (define-foreign %websocket-send
        "webSocket" "send"
        (ref extern) (ref extern) -> none)
      (define-foreign %set-websocket-on-open!
        "webSocket" "setOnOpen"
        (ref extern) (ref extern) -> none)
      (define-foreign %set-websocket-on-error!
        "webSocket" "setOnError"
        (ref extern) (ref extern) -> none)
      (define-foreign %set-websocket-on-message!
        "webSocket" "setOnMessage"
        (ref extern) (ref extern) -> none)
      (define-foreign %set-websocket-on-close!
        "webSocket" "setOnClose"
        (ref extern) (ref extern) -> none)
      (define-record-type <websocket>
        (wrap-websocket extern deq-ch)
        websocket?
        (extern unwrap-websocket)
        (deq-ch websocket-deq-ch))
      ;; The keyword args are unused but they are there to match
      ;; guile-websocket.
      (define* (open-websocket-for-uri uri-or-string #:key
                                       configure-socket
                                       verify-certificate?)
        (define url
          (match uri-or-string
            ((? string? url) url)
            ((? uri? uri) (uri->string uri))))
        (define extern (%open-websocket url))
        (define event-ch (make-channel))
        (define-values (enq-ch deq-ch stopped?)
          (spawn-delivery-agent))
        (define (on-open)
          (put-message event-ch 'opened))
        (define (on-error)
          (put-message event-ch 'error))
        (define (on-close code reason)
          (put-message enq-ch the-eof-object)
          (signal-condition! stopped?))
        (define (on-message data)
          (put-message enq-ch (array-buffer->bytevector data)))
        (%set-websocket-on-error! extern (procedure->external on-error))
        (%set-websocket-on-open! extern (procedure->external on-open))
        (%set-websocket-on-close! extern (procedure->external on-close))
        (%set-websocket-on-message! extern (procedure->external on-message))
        (match (get-message event-ch)
          ['opened 'noop]
          ['error (error (format #f "Failed to open new connection to ~a" uri-or-string))])
        (wrap-websocket extern deq-ch))
      (define (close-websocket ws)
        (%close-websocket (unwrap-websocket ws)))
      (define* (websocket-send ws data #:key mask?)
        (%websocket-send (unwrap-websocket ws)
                         (bytevector->uint8-array data)))
      (define* (websocket-receive ws #:key wait-for-port max-attempts echo-close?)
        (get-message (websocket-deq-ch ws)))))

    (define* (open-websocket designator uri #:key (verify-certificate? #t))
      (spawn-fibrous-vow
       (lambda ()
         (define (verify-designator ws)
           ;; Send some random bytes to the server.  The server
           ;; should return a signature for those bytes which we
           ;; will match against the public key encoded in the peer
           ;; designator.
           (let ((bv (strong-random-bytes 64)))
             (websocket-send ws bv)
             (let ((sig
                    (match (websocket-receive ws #:wait-for-port wait-for-port)
                      ((? bytevector? bv) (syrup-decode bv))
                      (_ #f))))
               (unless (and sig
                            (verify (captp-signature->crypto-signature sig)
                                    bv
                                    (bytevector->crypto-public-key
                                     (base32-decode designator))))
                 (close-websocket ws)
                 (error "WebSocket designator verification failed")))))
         (let ((ws (open-websocket-for-uri uri
                                           #:configure-socket use-nonblocking-i/o
                                           #:verify-certificate? verify-certificate?)))
           (verify-designator ws)
           ws))))

    (define-actor (^websocket-captp-io bcom ws server-side?)
      (let ((io (spawn ^read-write-io ws #:cleanup close-websocket)))
        (methods
         ((read-message unmarshallers)
          (<- io 'read
              (lambda (ws)
                (match (websocket-receive ws
                                          #:wait-for-port wait-for-port
                                          #:max-attempts 3
                                          #:echo-close? server-side?)
                  ((? bytevector? bv)
                   (syrup-decode bv #:unmarshallers unmarshallers))
                  (_
                   (<-np-extern io 'halt)
                   the-eof-object)))))
         ((write-message msg marshallers)
          (<-np io 'write
                (lambda (ws)
                  (let ((bv (syrup-encode msg #:marshallers marshallers)))
                    (websocket-send ws bv #:mask? (not server-side?))
                    #t)))))))

    (define* (hint-ref peer key #:optional default)
      (hashmap-ref (ocapn-peer-hints peer) key default))

    (cond-expand
     (guile
      (define-actor (^websocket-netlayer/complete bcom #:key host port url
                                                  encrypted?
                                                  max-peers verify-certificates?
                                                  tls-private-key tls-certificate
                                                  designator-key)
        #:portrait
        (lambda ()
          (list host assigned-port url max-peers encrypted? verify-certificates?
                tls-private-key tls-certificate
                (private-key->data designator-key)))
        #:restore
        (lambda (version host port url max-peers encrypted? verify-certificates?
                         tls-private-key tls-certificate designator-key)
          (spawn ^websocket-netlayer/complete
                 #:host host
                 #:port port
                 #:url url
                 #:max-peers max-peers
                 #:encrypted? encrypted?
                 #:verify-certificates? verify-certificates?
                 #:tls-private-key tls-private-key
                 #:tls-certificate tls-certificate
                 #:designator-key (data->private-key designator-key)))

        (define-values (conn-establisher-vow conn-establisher-resolver)
          (spawn-promise-and-resolver))
        (define listener
          (spawn ^listener designator-key conn-establisher-vow))
        (define server
          (spawn ^websocket-server
                 #:host host
                 #:port port
                 #:max-clients max-peers
                 #:tls-private-key tls-private-key
                 #:tls-certificate tls-certificate
                 #:listener listener))
        (define assigned-port
          ($$ server 'get-port))
        (define external-url
          (if url
              url
              (default-url host assigned-port encrypted?)))
        (define our-location
          (make-ocapn-peer 'websocket
                           (base32-encode
                            (captp-public-key->bytevector
                             (key-pair->public-key designator-key)))
                           (hashmap ("url" external-url))))
        (unless verify-certificates?
          (warn "TLS certificate verification is disabled"))
        (methods
         ((netlayer-name) 'websocket)
         ((self-location? other) (same-peer-location? our-location other))
         ((our-location) our-location)
         ((setup conn-establisher)
          ($$ conn-establisher-resolver 'fulfill conn-establisher))
         ((connect-to remote-peer)
          (match remote-peer
            (($ <ocapn-peer> 'websocket designator _)
             (let ((uri (string->uri (hint-ref remote-peer "url"))))
               ;; If the server is encrypted, then clients must be
               ;; encrypted, too.
               (when (and tls-private-key (not (eq? (uri-scheme uri) 'wss)))
                 (error "not a secure WebSocket URI" uri))
               (let-on ((ws (open-websocket
                             designator uri
                             #:verify-certificate? verify-certificates?)))
                 (<- conn-establisher-vow
                     (spawn ^websocket-captp-io ws #f)
                     remote-peer))))
            (($ <ocapn-peer> transport _ _)
             (error "mismatched netlayer" transport 'websocket))))
         ;; Private API:
         ((halt) (<- server 'halt))))

      (define-actor (^websocket-netlayer/generate-designator-key bcom swap-to-cell
                                                                 #:key
                                                                 host port url
                                                                 max-peers
                                                                 verify-certificates?
                                                                 encrypted?
                                                                 tls-private-key
                                                                 tls-certificate
                                                                 designator-key)
        (define-values (next-vow next-resolver)
          (spawn-promise-and-resolver))
        (define (fresh-key-pair)
          (spawn-fibrous-vow generate-key-pair))
        (let*-on ((swap-to (<- swap-to-cell))
                  (key (if swap-to (fresh-key-pair) #f)))
          (when key
            (let ((next (spawn ^websocket-netlayer/complete
                               #:host host
                               #:port port
                               #:url url
                               #:encrypted? encrypted?
                               #:max-peers max-peers
                               #:verify-certificates? verify-certificates?
                               #:tls-private-key tls-private-key
                               #:tls-certificate tls-certificate
                               #:designator-key key)))
              (<-np swap-to next)
              ($$ swap-to-cell #f)
              ($$ next-resolver 'fulfill next))))
        (match-lambda*
          (('netlayer-name) 'websocket)
          (args (apply <- next-vow args))))

      (define-actor (^websocket-netlayer/generate-tls-key bcom swap-to-cell #:key
                                                          host port url
                                                          encrypted?
                                                          max-peers
                                                          verify-certificates?
                                                          designator-key)
        (define-values (next-vow next-resolver)
          (spawn-promise-and-resolver))
        (define (fresh-self-signed-tls-cert)
          (spawn-fibrous-vow
           (lambda ()
             (let* ((key (generate-tls-private-key))
                    (cert (generate-tls-certificate key)))
               (list key cert)))))
        (let*-on ((swap-to (<- swap-to-cell))
                  (key+cert (if swap-to (fresh-self-signed-tls-cert) #f)))
          (match key+cert
            (#f 'noop)
            ((key cert)
             (let ((next (if designator-key
                             (spawn ^websocket-netlayer/complete
                                    #:host host
                                    #:port port
                                    #:url url
                                    #:encrypted? encrypted?
                                    #:max-peers max-peers
                                    #:verify-certificates? verify-certificates?
                                    #:tls-private-key key
                                    #:tls-certificate cert
                                    #:designator-key designator-key)
                             (spawn ^websocket-netlayer/generate-designator-key
                                    swap-to-cell
                                    #:host host
                                    #:port port
                                    #:url url
                                    #:encrypted? encrypted?
                                    #:max-peers max-peers
                                    #:verify-certificates? verify-certificates?
                                    #:encrypted? #t
                                    #:tls-private-key key
                                    #:tls-certificate cert))))
               (if designator-key
                   ($$ swap-to-cell #f)
                   (<-np swap-to next))
               ($$ next-resolver 'fulfill next)))))
        (match-lambda*
          (('netlayer-name) 'websocket)
          (args (apply <- next-vow args)))))
     (hoot
      (define-actor (^websocket-netlayer/client-only bcom)
        (define-values (conn-establisher-vow conn-establisher-resolver)
          (spawn-promise-and-resolver))
        (define our-location (make-ocapn-peer 'websocket "unreachable" (hashmap)))
        (methods
         ((netlayer-name) 'websocket)
         ((self-location? other) (same-peer-location? our-location other))
         ((our-location) our-location)
         ((setup conn-establisher)
          ($$ conn-establisher-resolver 'fulfill conn-establisher))
         ((connect-to remote-peer)
          (match remote-peer
            (($ <ocapn-peer> 'websocket designator _)
             (let ((url (hint-ref remote-peer "url")))
               (let-on ((ws (open-websocket designator url)))
                 (<- conn-establisher-vow
                     (spawn ^websocket-captp-io ws #f)
                     remote-peer))))
            (($ <ocapn-peer> transport _ _)
             (error "mismatched netlayer" transport 'websocket))))))))

    (define (default-url host port encrypted?)
      (uri->string
       (build-uri (if encrypted? 'wss 'ws)
                  #:host host
                  #:port port
                  #:validate? #t)))


    (define* (^websocket-netlayer bcom #:key
                                  (host "127.0.0.1")
                                  (port 0)
                                  (max-peers 32)
                                  (verify-certificates? #t)
                                  (encrypted? #t)
                                  url
                                  tls-private-key
                                  tls-certificate
                                  designator-key)
      "Spawn and return a new WebSocket netlayer.  The server component of
the netlayer is bound to @var{host} and @var{port}.  If @var{port} is
not specified, an available port is chosen automatically.

The @var{url} argument allows for specifying a custom URL for use as a
hint when generating OCapN sturdyrefs.  By default, the URL looks like
@code{ws(s)://$host:$port}. Custom URLs are often necessary, such as
in the case of a relay that uses @url{https://nginx.org/,nginx} as the
publicly accesible web server with a reverse proxy to the netlayer
bound on the loopback device.  @var{url} is also necessary when the
server is bound to a @var{host} address of @code{\"0.0.0.0\"} to
produce a usable address for the sturdyref hint.

@var{max-peers} determines how many peers can be simultaneously
connected to the netlayer; 32 by default.

The @var{encrypted?} flag determines if TLS encryption is used for
inbound connections (i.e. @code{wss://}).  Encryption is enabled by
default and a warning is emitted when it is disabled.

The @var{verify-certificates?} flag determines if the TLS certificates
of outbound peers should be verified upon connection.  Certification
verification is enabled by default and a warning is emitted when it is
disabled.

@var{tls-private-key} and @var{tls-certificate} specify the X.509
private key and certificate to use for encrypting inbound connections
with TLS.  If one or both are unspecified, they will be automatically
generated (provided that the version of Guile-GnuTLS in use is new
enough to do so.)  To import PEM encoded private keys and certificates
from the file system, use @code{load-tls-private-key} and
@code{load-tls-certificate}, respectively.

The @var{designator-key} is the private key used to authenticate the
designator used within the peer location, as created by the
@code{generate-key-pair} procedure in @code{(goblins utils crypto)}.
This key controls the identity of the netlayer.  By default, no
pre-existing key is provided an a fresh key will generated
automatically.

Finally, note that in the case of web deployment (via Guile Hoot) that
it is @emph{not possible} for the netlayer to start a WebSocket
server.  Thus, this netlayer @emph{must} be used in conjunction with a
relay netlayer in order to bootstrap web clients into an otherwise
peer-to-peer network."
      (cond-expand
       (guile
        (define-values (swap-to-vow swap-to-resolver)
          (spawn-promise-and-resolver))
        ;; TODO Remove me when aurie cleans up old objects.
        ;;
        ;; Aurie currently does not clean-up/GC any object. That means any
        ;; object that's been in the object graph will always be restord even if
        ;; it's not used. Normally this takes a bit more resources, but doesn't
        ;; cause any problems. However because the pre-setup websocket actors
        ;; have the swapper, they will set things up and then use their swap
        ;; power causing lots of havoc! As workaround, we make it a cell and the
        ;; pre-setup actors will take this power away from themselves when
        ;; they've handed off to the next object.
        (define swap-to-cell (spawn ^cell swap-to-vow))
        (define netlayer
          (if encrypted?
              (if (and tls-private-key tls-certificate)
                  (if designator-key
                      (spawn ^websocket-netlayer/complete
                             #:host host
                             #:port port
                             #:url url
                             #:encrypted? encrypted?
                             #:max-peers max-peers
                             #:verify-certificates? verify-certificates?
                             #:tls-private-key tls-private-key
                             #:tls-certificate tls-certificate
                             #:designator-key designator-key)
                      (spawn ^websocket-netlayer/generate-designator-key
                             swap-to-cell
                             #:host host
                             #:port port
                             #:url url
                             #:encrypted? encrypted?
                             #:max-peers max-peers
                             #:verify-certificates? verify-certificates?
                             #:tls-private-key tls-private-key
                             #:tls-certificate tls-certificate))
                  (spawn ^websocket-netlayer/generate-tls-key
                         swap-to-cell
                         #:host host
                         #:port port
                         #:url url
                         #:encrypted? encrypted?
                         #:max-peers max-peers
                         #:verify-certificates? verify-certificates?
                         #:designator-key designator-key))
              (if designator-key
                  (spawn ^websocket-netlayer/complete
                         #:host host
                         #:port port
                         #:url url
                         #:encrypted? encrypted?
                         #:max-peers max-peers
                         #:verify-certificates? verify-certificates?
                         #:designator-key designator-key)
                  (spawn ^websocket-netlayer/generate-designator-key
                         swap-to-cell
                         #:host host
                         #:port port
                         #:url url
                         #:max-peers max-peers
                         #:verify-certificates? verify-certificates?
                         #:encrypted? encrypted?))))
        (define-values (proxy swap-to)
          (swappable netlayer '^websocket-netlayer))
        ($$ swap-to-resolver 'fulfill swap-to)
        proxy)
       (hoot
        (unless verify-certificates? ; no choice on the web
          (error "TLS certificate verification must be enabled on Hoot"))
        (spawn ^websocket-netlayer/client-only))))

    (define websocket-netlayer-env
      (cond-expand
       (guile
        (persistence-env-compose
         (namespace-env (goblins ocapn netlayer websocket)
                        ^websocket-netlayer/generate-tls-key
                        ^websocket-netlayer/generate-designator-key
                        ^websocket-netlayer/complete)
         swappable-env))
       (hoot
        (namespace-env (goblins ocapn netlayer websocket)
                       ^websocket-netlayer/client-only))))))
