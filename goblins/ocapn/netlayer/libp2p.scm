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
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer utils)
  #:use-module (goblins ocapn netlayer base-port)
  #:use-module (goblins contrib syrup)
  #:export (new-libp2p-netlayer))

(define (build-path . args)
  (string-join args file-name-separator-string))

(define default-libp2p-control-path
  (build-path "/tmp" "libp2p-control.sock"))

(define default-libp2p-path
  (build-path "/tmp" "goblins-libp2p"))

(define (setup-ocapn-io control-path path)
  ;; Set up the temporary directory and paths we'll be using for this
  ;; captp process
  (unless (file-exists? path)
    ;; TODO: Make this recursive?
    (mkdir path))
  (define incoming-connections-path
    (random-tmp-filename path
                         #:format-name
                         (lambda (name)
                           ;; store the uid in the ocapn sock directory so maybe
                           ;; we could add a gc routine for obviously-unused
                           ;; old sock files
                           (format #f "ocapn-~a-~a.sock"
                                   (getuid) name))))
  (define outgoing-connections-path
    (random-tmp-filename path
                         #:format-name
                         (lambda (name)
                           ;; store the uid in the ocapn sock directory so maybe
                           ;; we could add a gc routine for obviously-unused
                           ;; old sock files
                           (format #f "ocapn-~a-~a.sock"
                                   (getuid) name))))

  (define incoming-connections-sock
    (make-server-unix-domain-socket incoming-connections-path))

  (define control-sock
    (make-client-unix-domain-socket control-path))

  (define-values (control-in-ch control-out-ch)
    (line-delimited-ports->channels control-sock control-sock))

  (put-message control-out-ch
               (format #f "NEW incoming:~a outgoing:~a protocol:ocapn version:1.0.0"
                       incoming-connections-path
                       outgoing-connections-path))
  (define our-location
    (match (string-split (get-message control-in-ch) #\space)
      [("Address:" designator)
       (make-ocapn-node 'libp2p (designator->encoded-designator designator) #f)]
      [something (error "Got unknown:" something)]))

  (values our-location control-sock
          incoming-connections-sock
          outgoing-connections-path))

(define (designator->encoded-designator designator)
  (url-base64-encode
   (string->bytevector designator "utf-8")))

(define (encoded-designator->designator designator)
  (bytevector->string
   (url-base64-decode designator)
   "utf-8"))

(define (new-libp2p-netlayer)
  (define-values (our-location control-sock incoming-conn-sock outgoing-conn-path)
    (setup-ocapn-io default-libp2p-control-path
                    default-libp2p-path))
  (spawn ^libp2p-netlayer
         our-location
         incoming-conn-sock
         outgoing-conn-path))

(define (setup-outgoing-sock sock location)
  (display
   (format #f "CONNECT ~a\n"
           (encoded-designator->designator
            (ocapn-node-designator location)))
   sock)
  (flush-output-port sock))

(define (^libp2p-netlayer bcom our-location
                          incoming-connection-sock
                          outgoing-connection-path)
  (define (incoming-accept)
    (match (accept incoming-connection-sock SOCK_NONBLOCK)
      ((client . addr)
       (setvbuf client 'block 1024)
       client)))
  
  (define (outgoing-connect-location location)
    (unless (eq? (ocapn-node-transport location) 'libp2p)
      (error "Wrong netlayer! Expected libp2p" location))
    (let* ((designator (ocapn-node-designator location))
           (sock (make-client-unix-domain-socket outgoing-connection-path)))
      (setup-outgoing-sock sock location)
      sock))
  (^base-port-netlayer bcom our-location
                       incoming-accept outgoing-connect-location))
