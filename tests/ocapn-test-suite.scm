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

(use-modules (goblins)
             (goblins utils hashmap)
             (goblins actor-lib io)
             (goblins actor-lib on)
             (goblins actor-lib methods)
             (goblins ocapn ids)
             (goblins ocapn captp)
             (goblins ocapn netlayer base-port)
             (goblins ocapn netlayer onion)
             (goblins utils crypto)
             (goblins utils base32)
             (ice-9 match)
             (ice-9 iconv)
             (srfi srfi-11)
             (fibers conditions))

(define peer-locator-output-file
  (getenv "GOBLINS_OCAPN_PEER_LOCATOR"))

;; Provide an ability for the user to choose the netlayer
(define chosen-netlayer
  (match (command-line)
    ((_ "tcp-testing-only") 'tcp-testing-only)
    ((_ "onion") 'onion)
    ((_) 'tcp-testing-only)))

;; Copied verbatim from TCP-TLS netlayer
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
;; -----------------------------

(define-actor (^tcp-testing-netlayer bcom host #:optional [port 0])
  (define-values (sock assigned-port)
    (make-server-socket+port port 32))
  (define socket-io
    (spawn ^io sock #:cleanup close-port))

  (define our-location
    (make-ocapn-peer 'tcp-testing-only
                     "guile-goblins"
                     (hashmap ("host" host)
                              ("port" (number->string assigned-port)))))

  (define (incoming-accept)
    (on-match (<- socket-io
                  (lambda (resource)
                    (accept resource O_NONBLOCK)))
        ((client-socket . _)
         (setvbuf client-socket 'block)
         (use-nonblocking-i/o client-socket)
         client-socket)))

  (define (outgoing-connect-to-loc loc)
    (unless (eq? (ocapn-peer-transport loc) 'tcp-testing-only)
      (error "Wrong netlayer! Expected `tcp-testing-only'" loc))
    (let*-values (((hints) (ocapn-peer-hints loc))
                  ((host) (hashmap-ref hints "host"))
                  ((port) (hashmap-ref hints "port")))
      (make-client-socket host (string->number port))))

  (define main-beh
    (^base-port-netlayer bcom our-location incoming-accept
                         outgoing-connect-to-loc))

  (extend-methods main-beh
   [(halt)
    ($ socket-io 'halt)]))

(define (trigger-gc)
  (define (^gc-collect _bcom)
    (lambda ()
      (sleep 1)
      (gc)))

  (define self (spawn ^gc-collect))
  (<-np self))

(define (^car _bcom color model)
  (lambda ()
    (format #f "Vroom! I am a ~a ~a car!" color model)))

(define (^car-factory _bcom)
  (lambda car-specs
    (define cars
      (map
       (lambda (car-spec)
               (apply spawn ^car car-spec))
       car-specs))
    (apply values cars)))

(define (^car-factory-builder _bcom)
  (lambda ()
    (spawn ^car-factory)))

(define (^echo _bcom)
  (lambda args
    (trigger-gc)
    args))

(define (^greeter _bcom)
  (lambda (target)
    (on (<- target "Hello")
        (lambda result
          (trigger-gc)))))

(define (^promise-resolver _bcom)
  (lambda ()
    (define-values (vow resolver)
      (spawn-promise-and-resolver))
    (list vow resolver)))

(define (^sturdyref-provider _bcom mycapn)
  (lambda (sref)
    (pk 'enlivening sref)
    ($ mycapn 'enliven sref)))

(define a-vat (spawn-vat))
(with-vat a-vat
  (define testing-netlayer
    (match chosen-netlayer
      ['tcp-testing-only
       (spawn ^tcp-testing-netlayer "localhost")]
      ['onion
       (spawn ^onion-netlayer)]))
  (define mycapn (spawn-mycapn testing-netlayer))
  (define nonce-registry ($ mycapn 'get-registry))

  (define car-factory-builder (spawn ^car-factory-builder))
  (define car-factory-builder-swiss-num
    (string->bytevector "JadQ0++RzsD4M+40uLxTWVaVqM10DcBJ" "ascii"))
  ($ nonce-registry 'register car-factory-builder car-factory-builder-swiss-num)

  (define echo (spawn ^echo))
  (define echo-swiss-num
    (string->bytevector "IO58l1laTyhcrgDKbEzFOO32MDd6zE5w" "ascii"))
  ($ nonce-registry 'register echo echo-swiss-num)

  (define greeter (spawn ^greeter))
  (define greeter-swiss-num
    (string->bytevector "VMDDd1voKWarCe2GvgLbxbVFysNzRPzx" "ascii"))
  ($ nonce-registry 'register greeter greeter-swiss-num)

  (define promise-resolver (spawn ^promise-resolver))
  (define promise-resolver-swiss-num
    (string->bytevector "IokCxYmMj04nos2JN1TDoY1bT8dXh6Lr" "ascii"))
  ($ nonce-registry 'register promise-resolver promise-resolver-swiss-num)

  (define sturdyref-provider (spawn ^sturdyref-provider mycapn))
  (define sturdyref-provider-swiss-num
    (string->bytevector "gi02I1qghIwPiKGKleCQAOhpy3ZtYRpB" "ascii"))
  ($ nonce-registry 'register sturdyref-provider sturdyref-provider-swiss-num)

  (on (<- testing-netlayer 'our-location)
      (lambda (loc)
        (when peer-locator-output-file
          (call-with-output-file peer-locator-output-file
            (lambda (port)
              (display (ocapn-id->string loc) port))))
        (format #t "Connect test suite to: ~a\n" (ocapn-id->string loc)))))

(define forever (make-condition))
(wait forever)
