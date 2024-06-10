;;; Copyright 2023 Christine Lemmer-Webber
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

(define-module (tests ocapn netlayer test-prelay)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer fake)
  #:use-module (goblins ocapn netlayer prelay)
  #:use-module (goblins actor-lib facet)
  #:use-module (tests utils)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(test-begin "test-prelay")

(define fakenl-vat (spawn-vat #:name "fakenl-vat"))
(define fakenl-network
  (with-vat fakenl-vat
    (spawn ^fake-network)))

(define (spawn-vat-in-fakenl name)
  (define new-vat (spawn-vat #:name (string-append name "-vat")))
  (define new-conn-ch (make-channel))
  (define location (string->ocapn-id (format #f "ocapn://~a.fake" name)))
  (define netlayer
    (with-vat new-vat
      (spawn ^fake-netlayer name fakenl-network new-conn-ch)))
  (with-vat fakenl-vat
    ($ fakenl-network 'register name new-conn-ch))
  (define mycapn
    (with-vat new-vat (spawn-mycapn netlayer)))
  (values new-vat location netlayer mycapn))

(define-values (a-vat a-loc a-netlayer a-mycapn)
  (spawn-vat-in-fakenl "alice-m"))
(define-values (b-vat b-loc b-netlayer b-mycapn)
  (spawn-vat-in-fakenl "bob-m"))
(define-values (relay-vat relay-loc relay-netlayer relay-mycapn)
  (spawn-vat-in-fakenl "relay"))

(define (^greeter _bcom my-name)
  (lambda (your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name)))

(define bob-greeter
  (with-vat b-vat
    (spawn ^greeter "Bob")))

;;; Setting up endpoints and controllers for Alice/Bob on the relay

(define-values (ra-endpoint ra-controller)
  (with-vat relay-vat
    (spawn-prelay-pair (spawn ^facet relay-mycapn 'enliven))))

(define-values (ra-endpoint-sref ra-controller-sref)
  (with-vat relay-vat
    (values ($ relay-mycapn 'register ra-endpoint 'fake)
            ($ relay-mycapn 'register ra-controller 'fake))))

(define-values (rb-endpoint rb-controller)
  (with-vat relay-vat
    (spawn-prelay-pair (spawn ^facet relay-mycapn 'enliven))))

(define-values (rb-endpoint-sref rb-controller-sref)
  (with-vat relay-vat
    (values ($ relay-mycapn 'register rb-endpoint 'fake)
            ($ relay-mycapn 'register rb-controller 'fake))))

;;; Now to create the prelay and register it with Alice and Bob's mycapns
(define a-prelay-netlayer
  (with-vat a-vat
    (spawn ^prelay-netlayer
           (spawn ^facet a-mycapn 'enliven)
           ra-endpoint-sref
           ra-controller-sref)))

(with-vat a-vat
  ($ a-mycapn 'install-netlayer a-prelay-netlayer))

(define b-prelay-netlayer
  (with-vat b-vat
    (spawn ^prelay-netlayer
           (spawn ^facet a-mycapn 'enliven)
           rb-endpoint-sref
           rb-controller-sref)))

(with-vat b-vat
  ($ b-mycapn 'install-netlayer b-prelay-netlayer))

(define bob-greeter-sref
  (with-vat b-vat
    ($ b-mycapn 'register bob-greeter 'prelay)))

(test-assert "Prelay netlayer sturdyref resolves to a remote reference"
  (match (resolve-vow-and-return-result
          a-vat
          (lambda ()
            ($ a-mycapn 'enliven bob-greeter-sref)))
    (#(ok (? remote-object-refr?)) #t)
    (_ #f)))

(test-assert "Simple messaging over the prelay netlayer works"
  (match (resolve-vow-and-return-result
          a-vat
          (lambda ()
            (<- ($ a-mycapn 'enliven bob-greeter-sref) "Alice")))
    (#(ok "Hello Alice, my name is Bob!") #t)
    (_ #f)))

(test-end "test-prelay")
