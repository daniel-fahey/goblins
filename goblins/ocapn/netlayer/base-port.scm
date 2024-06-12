;;; Copyright 2021-2022 Christine Lemmer-Webber
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

(define-module (goblins ocapn netlayer base-port)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 iconv)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins inbox)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer utils)
  #:use-module (goblins contrib syrup)
  #:export (^base-port-netlayer))

(define (^base-port-netlayer _bcom our-location
                             incoming-accept
                             outgoing-connect-location)
  "A basis for defining netlayers."
  (define our-netlayer-name
    (ocapn-node-transport our-location))
  (define-values (conn-establisher-vow conn-establisher-resolver)
    (spawn-promise-values))

  (define (listen-and-handle-new-connection conn-establisher)
    (on (incoming-accept)
        (lambda (incoming-port)
          (<-np conn-establisher (spawn ^captp-io incoming-port) #f)
          (listen-and-handle-new-connection conn-establisher))))

  (methods
   [(netlayer-name) our-netlayer-name]
   [(our-location) our-location]
   [(self-location? loc)
    (same-node-location? our-location loc)]
   [(setup conn-establisher)
    (<-np conn-establisher-resolver 'fulfill conn-establisher)
    (listen-and-handle-new-connection conn-establisher)]
   [(connect-to remote-node)
    (unless (eq? (ocapn-node-transport remote-node)
                 our-netlayer-name)
      (error "Mismatched netlayer:"
             (ocapn-node-transport remote-node)
             our-netlayer-name))
    ;; Asynchronously set up connection.  Once it's ready, we'll
    ;; return the value from the connection establisher
    ;; (which itself returns the meta-bootstrap-vow)
    (define connected-port-vow
      (spawn-fibrous-vow
       (lambda ()
         (outgoing-connect-location remote-node))))
    
    (on connected-port-vow
        (lambda (connected-port)
          (<- conn-establisher-vow (spawn ^captp-io connected-port) remote-node))
        #:promise? #t)]))
