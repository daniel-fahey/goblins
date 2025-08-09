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

(define-module (goblins utils unix-domain-socket)
  #:use-module (bstructs)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (goblins contrib syrup)
  #:use-module (goblins ocapn ids)
  #:use-module (fibers)
  #:use-module (fibers operations)
  #:use-module (fibers conditions)
  #:use-module (fibers io-wakeup)
  #:export (make-unix-domain-socket
            send-port-over-socket
            read-port-from-socket
            ;; UDS types
            <uds:register>
            make-uds:register
            <uds:signature>
            make-uds:signature
            <uds:server-response>
            make-uds:server-response
            <uds:new-connection>
            make-uds:new-connection
            marshallers
            unmarshallers))

;; The following is translated from <bits/socket.h>
(define SCM_RIGHTS #x01)
(define-bstruct socklen_t uint32)
(define-bstruct iovec
  (struct
   (iov_base (* void))
   (iov_len size_t)))
(define-bstruct msghdr
  (struct
   (msg_name (* void))
   (msg_namelen socklen_t)
   (msg_iov (* iovec))
   (msg_iovlen size_t)
   (msg_control (* void))
   (msg_controllen size_t)
   (msg_flags int)))
(define-bstruct cmsghdr
  (struct
   (cmsg_len size_t)
   (cmsg_level int)
   (cmsg_type int)))
(define-bstruct control-message
  (struct
   (header cmsghdr)
   (fd int)))

;; Helper to make control message buffers
(define (make-control-message)
  (bstruct-alloc control-message
                 (-> header
                     (cmsg_len (bstruct-sizeof control-message))
                     (cmsg_level SOL_SOCKET)
                     (cmsg_type SCM_RIGHTS))))

;; libc bindings.
(define libc (dynamic-link))
(define %sendmsg
  (foreign-library-function libc "sendmsg"
                            #:arg-types (list int '* int)
                            #:return-type ssize_t
                            #:return-errno? #t))
(define (sendmsg sock header flags)
  (call-with-values (lambda ()
                      (%sendmsg (port->fdes sock)
                                (bstruct->pointer msghdr header)
                                flags))
    (lambda (n errno)
      (if (zero? errno)
          n
          (error "sendmsg failed" errno)))))
(define %recvmsg
  (foreign-library-function libc "recvmsg"
                            #:arg-types (list int '* int)
                            #:return-type ssize_t
                            #:return-errno? #t))
(define (recvmsg sock msg flags)
  ;; We need to ensure the fiber will suspend until the socket
  ;; is readable before we calling %recvmsg to avoid EAGAIN/EWOULDBLOCK.
  (perform-operation (wait-until-port-readable-operation sock))
  (call-with-values (lambda ()
                      (%recvmsg (port->fdes sock)
                                (bstruct->pointer msghdr msg)
                                flags))
    (lambda (n errno)
      (if (zero? errno)
        n
        (errno (error "recvmsg failed" errno))))))

(define (send-port-over-socket sock port)
  ;; We have to send some primary data in order to send ancillary
  ;; data, so we send some garbage that the server will ignore.
  (define fdes (port->fdes port))
  (define data (s32vector 12345))
  (define iov
    (bstruct-alloc iovec
                  (iov_base (bytevector->pointer data))
                  (iov_len (bytevector-length data))))
  (define control-header
    (bstruct-alloc cmsghdr
                   (cmsg_len (bstruct-sizeof control-message))
                   (cmsg_level SOL_SOCKET)
                   (cmsg_type SCM_RIGHTS)))
  (define control (make-control-message))
  (bstruct-set! control-message control (fd (port->fdes port)))
  (define msg
    (bstruct-alloc msghdr
                   ;; Don't need to specify outgoing address.
                   (msg_name %null-pointer)
                   (msg_namelen 0)
                   ;; Primary message (garbage, in this case)
                   (msg_iov (bstruct->pointer iovec iov))
                   (msg_iovlen 1)
                   ;; Ancillary data (holds the file descriptor)
                   (msg_control (bstruct->pointer control-message control))
                   (msg_controllen (bstruct-sizeof control-message))
                   (msg_flags 0)))
  (sendmsg sock msg 0))

(define (read-port-from-socket sock)
  (define data (make-s32vector 1)) ; ignored
  (define iov
    (bstruct-alloc iovec
                   (iov_base (bytevector->pointer data))
                   (iov_len (bytevector-length data))))
  (define control (make-control-message))
  (define msg
    (bstruct-alloc msghdr
                   (msg_name %null-pointer)
                   (msg_namelen 0)
                   (msg_iov (bstruct->pointer iovec iov))
                   (msg_iovlen 1)
                   (msg_control (bstruct->pointer control-message control))
                   (msg_controllen (bstruct-sizeof control-message))
                   (msg_flags 0)))

  (recvmsg sock msg 0)
  (define fd (bstruct-ref control-message control fd))
  (fdopen fd "rb+"))

(define (make-unix-domain-socket)
  (socket PF_UNIX (logior SOCK_STREAM SOCK_CLOEXEC) 0))

;; Types used for the unix domain socket netlayer
(define-syrup-record-type <uds:register>
  (make-uds:register peer-location client-challenge)
  uds:register?
  uds:register marshall::uds:register unmarshall::uds:register
  (peer-location uds:register-peer-location)
  (client-challenge uds:register-client-challenge))

(define-syrup-record-type <uds:server-response>
  (make-uds:server-response pubkey server-challenge client-challenge-response)
  uds:server-response?
  uds:server-response marshall::uds:server-response unmarshall::uds:server-response
  (pubkey uds:server-response-pubkey)
  (server-challenge uds:server-response-server-challenge)
  (client-challenge-response uds:server-response-client-challenge-response))

(define-syrup-record-type <uds:signature>
  (make-uds:signature payload signature)
  uds:signature?
  uds:signature marshall::uds:signature unmarshall::uds:signature
  (payload uds:signature-payload)
  (signature uds:signature-signature))

(define-syrup-record-type <uds:new-connection>
  (make-uds:new-connection from to)
  uds:new-connection?
  uds:new-connection marshall::uds:new-connection unmarshall::uds:new-connection
  (from uds:new-connection-from)
  (to uds:new-connection-to))

(define marshallers
  (list marshall::uds:register
        marshall::uds:server-response
        marshall::uds:signature
        marshall::uds:new-connection
        marshall::ocapn-peer))
(define unmarshallers
  (list unmarshall::uds:register
        unmarshall::uds:server-response
        unmarshall::uds:signature
        unmarshall::uds:new-connection
        unmarshall::ocapn-peer))
