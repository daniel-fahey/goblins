;;; Copyright 2022 Christine Lemmer-Webber
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

(define-module (goblins ocapn netlayer testuds)
  #:use-module (goblins)
  #:use-module (goblins ocapn netlayer base-port)
  #:use-module (goblins ocapn netlayer utils)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn structs-urls)
  #:use-module (goblins utils random-name)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 curried-definitions)
  #:export (^testuds-netlayer))

(define* (^testuds-netlayer bcom netlayers-dir
                            #:key (machine-id (random-name 32)))
  (define our-location
    (make-ocapn-machine 'testuds machine-id '()))
  (define (sock-filename-for-id id)
    (string-append netlayers-dir file-name-separator-string id
                   ".sock"))
  (define our-sock-filename
    (sock-filename-for-id machine-id))
  (define our-server-sock
    (make-server-unix-domain-socket (sock-filename-for-id machine-id)))
  (define (incoming-accept)
    (match (accept our-server-sock SOCK_NONBLOCK)
      ((client . addr)
       (setvbuf client 'block 1024)
       ;; (As said in the Fibers manual:)
       ;; Disable Nagle's algorithm.  We buffer ourselves.
       ;; (setsockopt client IPPROTO_TCP TCP_NODELAY 1)
       client)))
  (define (outgoing-connect-location location)
    (unless (eq? (ocapn-machine-transport location) 'testuds)
      (error "Wrong netlayer! Expected testuds" location))
    (make-client-unix-domain-socket (sock-filename-for-id
                                     (ocapn-machine-address location))))
  (^base-port-netlayer bcom our-location
                       incoming-accept outgoing-connect-location))
