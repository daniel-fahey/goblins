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
  #:use-module (goblins ocapn ids)
  #:use-module (goblins utils random-name)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 curried-definitions)
  #:export (^testuds-netlayer))

(define* (^testuds-netlayer bcom netlayers-dir
                            #:key (peer-id (random-name 32)))
  (define our-location
    (make-ocapn-peer 'testuds peer-id '()))
  (define (sock-filename-for-id id)
    (string-append netlayers-dir file-name-separator-string id
                   ".sock"))
  (define our-sock-filename
    (sock-filename-for-id peer-id))
  (define our-server-sock
    (make-server-unix-domain-socket (sock-filename-for-id peer-id)))
  (define (incoming-accept)
    (match (accept our-server-sock SOCK_NONBLOCK)
      ((client . addr)
       (setvbuf client 'block 1024)
       client)))
  (define (outgoing-connect-location location)
    (unless (eq? (ocapn-peer-transport location) 'testuds)
      (error "Wrong netlayer! Expected testuds" location))
    (make-client-unix-domain-socket (sock-filename-for-id
                                     (ocapn-peer-designator location))))
  (^base-port-netlayer bcom our-location
                       incoming-accept outgoing-connect-location))
