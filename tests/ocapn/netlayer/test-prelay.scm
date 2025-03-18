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
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins actor-lib facet)
  #:use-module (tests utils)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
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

(define (setup-prelay name)
  "Sets up both own prelay server and prelay client"
  (define-values (vat loc netlayer mycapn)
    (spawn-vat-in-fakenl (string-append "relay-" name)))
  (define-values (endpoint controller)
    (with-vat vat
      (spawn-prelay-pair (spawn ^facet mycapn 'enliven))))
  (define-values (endpoint-sref controller-sref)
    (with-vat vat
      (values ($ mycapn 'register endpoint 'fake)
              ($ mycapn 'register controller 'fake))))
  (define-values (client-vat client-loc client-netlayer client-mycapn)
    (spawn-vat-in-fakenl (string-append "client-" name)))
  (define prelay-netlayer
    (with-vat client-vat
      (spawn ^prelay-netlayer
             (spawn ^facet client-mycapn 'enliven)
             endpoint-sref
             controller-sref)))
  (with-vat client-vat
    ($ client-mycapn 'install-netlayer prelay-netlayer))
  (values client-vat client-mycapn client-netlayer netlayer))


;; Register the prelay with each side
(define-values (c-vat c-mycapn _cc-netlayer _cs-netlayer)
  (setup-prelay "carol"))
(define-values (d-vat d-mycapn _dd-netlayer _ds-netlayer)
  (setup-prelay "debra"))

(define c-greeter
  (with-vat c-vat
    (spawn ^greeter "Carol")))

(define c-greeter-sref-vow
  (with-vat c-vat
    ($ c-mycapn 'register c-greeter 'prelay)))

(define d-greeter-vow
  (with-vat d-vat
    (<- d-mycapn 'enliven c-greeter-sref-vow)))

(test-assert "Federated Prelay netlayer sturdyref resolves to a remote reference"
  (match (resolve-vow-and-return-result
          d-vat
          (lambda ()
            ($ d-mycapn 'enliven c-greeter-sref-vow)))
    (#(ok (? remote-object-refr?)) #t)
    (_ #f)))

(test-assert "Simple messaging over the federated prelay netlayer works"
  (match (resolve-vow-and-return-result
          d-vat
          (lambda ()
            (<- ($ d-mycapn 'enliven c-greeter-sref-vow) "Debra")))
    (#(ok "Hello Debra, my name is Carol!") #t)
    (_ #f)))

;; Test on-sever
;; The first case we're going to test is when alice on A is connected to her
;; prelay and bob on b is connected on his relay, if alice severs her connection
;; to her prelay, her refrs and bob's refrs should trigger on-sever using the
;; prelay.

(define-values (e-vat e-mycapn ec-netlayer es-netlayer)
  (setup-prelay "elsa"))

(define-values (f-vat f-mycapn fc-netlayer fs-netlayer)
  (setup-prelay "frank"))

(define elsa-greeter
  (with-vat e-vat
    (spawn ^greeter "Elsa")))
(define elsa-greeter-sref
  (with-vat e-vat
    (<- e-mycapn 'register elsa-greeter 'prelay)))

(define frank-greeter
  (with-vat f-vat
    (spawn ^greeter "Frank")))
(define frank-greeter-sref
  (with-vat f-vat
    (<- f-mycapn 'register frank-greeter 'prelay)))

;; Due to https://codeberg.org/spritely/goblins/issues/659
;; we should enliven ensure we don't end up in a crossed hellos situation, in
;; order to avoid it, we'll enliven one, then enliven the other.
(define-values (e-done-enlivening-vow e-done-enlivening-resolver)
  (with-vat e-vat
    (spawn-promise-and-resolver)))
(define frank-on-e-vow
  (with-vat e-vat
    (on (<- e-mycapn 'enliven frank-greeter-sref)
        (lambda (frank-greeter)
          ($ e-done-enlivening-resolver 'fulfill #t)
          frank-greeter)
        #:promise? #t)))
(define elsa-on-f-vow
  (with-vat f-vat
    (on e-done-enlivening-vow
        (lambda _
          (<- f-mycapn 'enliven elsa-greeter-sref))
        #:promise? #t)))

;; Break e's netlayer to her prelay
(define test-vat (spawn-vat))
(define-values (frank-on-e-severed-vow frank-on-e-resolver)
  (with-vat test-vat
    (spawn-promise-and-resolver)))
(define-values (elsa-on-f-severed-vow elsa-on-f-resolver)
  (with-vat test-vat
    (spawn-promise-and-resolver)))

(with-vat test-vat
  (on (all-of frank-on-e-vow elsa-on-f-vow)
      (match-lambda
        ((frank-on-e elsa-on-f)
         (on-sever elsa-on-f
                   (lambda (type reason)
                     ($ elsa-on-f-resolver 'fulfill (list type reason))))
         (on-sever frank-on-e
                   (lambda (type reason)
                     ($ frank-on-e-resolver 'fulfill (list type reason))))
         (<-np ec-netlayer 'halt)))))

(test-equal "When prelay client netlayer halts, serverence works for clients refrs"
  #(ok (disconnect "Remote disconnected"))
  (resolve-vow-and-return-result
   test-vat
   (lambda ()
     frank-on-e-severed-vow)))

(test-equal "When otherside prelay client netlayer disconnects, serverence works for our clients refrs"
  #(ok (disconnect "Remote disconnected"))
  (resolve-vow-and-return-result
   test-vat
   (lambda ()
     elsa-on-f-severed-vow)))

;; Test when the two prelays disconnect
(define-values (g-vat g-mycapn gc-netlayer gs-netlayer)
  (setup-prelay "gary"))

(define-values (h-vat h-mycapn hc-netlayer hs-netlayer)
  (setup-prelay "hannah"))

(define gary-greeter
  (with-vat g-vat
    (spawn ^greeter "Gary")))
(define gary-greeter-sref
  (with-vat g-vat
    (<- g-mycapn 'register gary-greeter 'prelay)))

(define hannah-greeter
  (with-vat h-vat
    (spawn ^greeter "Hannah")))
(define hannah-greeter-sref
  (with-vat h-vat
    (<- h-mycapn 'register hannah-greeter 'prelay)))

;; Like above, avoid crossed hellos for now.
(define-values (h-done-enlivening-vow h-done-enlivening-resolver)
  (with-vat h-vat
    (spawn-promise-and-resolver)))
(define hannah-on-g-vow
  (with-vat g-vat
    (on (<- g-mycapn 'enliven hannah-greeter-sref)
        (lambda (hannah-greeter)
          (<-np h-done-enlivening-resolver 'fulfill #t)
          hannah-greeter)
        #:promise? #t)))
(define gary-on-h-vow
  (with-vat h-vat
    (on h-done-enlivening-vow
        (lambda _
          (<- h-mycapn 'enliven gary-greeter-sref))
        #:promise? #t)))

(define-values (hannah-on-g-severed-vow hannah-on-g-resolver)
  (with-vat test-vat
    (spawn-promise-and-resolver)))
(define-values (gary-on-h-severed-vow gary-on-h-resolver)
  (with-vat test-vat
    (spawn-promise-and-resolver)))

(with-vat test-vat
  (on (all-of hannah-on-g-vow gary-on-h-vow)
      (match-lambda
        ((hannah-on-g gary-on-h)
         (on-sever hannah-on-g
                   (lambda (type reason)
                     ($ hannah-on-g-resolver 'fulfill (list type reason))))
         (on-sever gary-on-h
                   (lambda (type reason)
                     ($ gary-on-h-resolver 'fulfill (list type reason))))
         ;; Halt the netlayer gary's relay is using, this should break the connections
         ;; between the prelays
         (<-np gs-netlayer 'halt)))))

(test-equal "When sever happens between two federating prelay servers, on-sever should reach clients"
  #(ok ((disconnect "Remote disconnected") (disconnect "Remote disconnected")))
  (resolve-vow-and-return-result
   test-vat
   (lambda ()
     (all-of elsa-on-f-severed-vow frank-on-e-severed-vow))))

;; We want to verify that a netlayer client (^prelay-netlayer) will automatically
;; attempt to reconnect if a severence happens, since we can't restart the fake
;; netlayer, we'll have the severence happen at the server side and reconstruct
;; the prelay server.
(define-values (reconnect-server-vat reconnect-server-loc
                                     reconnect-server-netlayer
                                     reconnect-server-mycapn)
  (spawn-vat-in-fakenl "reconnect-prelay"))
(define-values (reconnect-endpoint reconnect-controller)
  (with-vat reconnect-server-vat
    (spawn-prelay-pair (spawn ^facet reconnect-server-mycapn 'enliven))))
(define-values (reconnect-endpoint-sref reconnect-controller-sref)
  (with-vat reconnect-server-vat
    (values ($ reconnect-server-mycapn 'register reconnect-endpoint 'fake)
            ($ reconnect-server-mycapn 'register reconnect-controller 'fake))))
(define-values (reconnect-client-vat reconnect-client-loc
                                     reconnect-client-netlayer
                                     reconnect-client-mycapn)
  (spawn-vat-in-fakenl "reconnect-netlayer"))
(define prelay-netlayer
  (with-vat reconnect-client-vat
    (spawn ^prelay-netlayer
           (spawn ^facet reconnect-client-mycapn 'enliven)
           reconnect-endpoint-sref
           reconnect-controller-sref)))
(with-vat reconnect-client-vat
  ($ reconnect-client-mycapn 'install-netlayer prelay-netlayer))

;; Make an object that we will be able to connect to.
(define reconnect-greeter
  (with-vat reconnect-client-vat
    (spawn ^greeter "Reconnect")))

(define reconnect-greeter-sref
  (with-vat reconnect-client-vat
    ($ reconnect-client-mycapn 'register reconnect-greeter 'prelay)))

;; To properly test we've reconnected later on, we of course need to connect
;; first to ensure there actually is a connection between
;; client <-> prelay server
;; Once there is we should then halt the server's prelay netlayer and make a
;, new one as explained below. Make a throw away client to get the sturdyref
;; then check we've halted the prelay server netlayer.
(define halted-server-netlayer?
  (let-values (((vat mycapn client-netlayer server-netlayer)
                (setup-prelay "throwaway")))
    (with-vat vat
      (on (<- mycapn 'enliven reconnect-greeter-sref)
          (lambda (greeter)
            ;; We now know the client and server are connected, as we have the refr.
            ;; now just wait until we've halted
            (<- reconnect-server-netlayer 'halt))
          #:promise? #t))))

;; We need to do this manually, not using spawn-vat-in-fakenl because if we setup
;; and install ourselves before the above happens, we clobber them causing
;; problems.
(define reconnect-server-vat*
  (spawn-vat #:name "reconnect-prelay*"))
(define reconnect-server-new-conn-ch
  (make-channel))
(define-values (reconnect-server-netlayer* reconnect-server-mycapn*)
  (let ((loc "ocapn://reconnect-prelay.fake")
        (netlayer (with-vat reconnect-server-vat*
                    (spawn ^fake-netlayer "reconnect-prelay" fakenl-network
                          reconnect-server-new-conn-ch))))
    (with-vat reconnect-server-vat*
      (values netlayer
              (spawn-mycapn netlayer)))))

(define reconnect-ready?
  (with-vat reconnect-server-vat*
    (on halted-server-netlayer?
        (lambda _
          ;; We're ready to clobber, install ourselves in the network as "reconnect-prelay"
          (<-np fakenl-network 'register "reconnect-prelay" reconnect-server-new-conn-ch)

          ;; As we cannot restart the netlayer, we need to make a new one and install
          ;; the endpoint and controller at the same place we had them before.
          (define-values (reconnect-endpoint* reconnect-controller*)
            (spawn-prelay-pair (spawn ^facet reconnect-server-mycapn* 'enliven)))

          (define reconnect-server-registry
            ($ reconnect-server-mycapn* 'get-registry))
          (on (all-of reconnect-endpoint-sref reconnect-controller-sref)
              (match-lambda
                ((endpoint-sref controller-sref)
                 (define endpoint-swiss-num
                   (ocapn-sturdyref-swiss-num endpoint-sref))
                 (define controller-swiss-num
                   (ocapn-sturdyref-swiss-num controller-sref))
                 ($ reconnect-server-registry 'register reconnect-endpoint* endpoint-swiss-num)
                 ($ reconnect-server-registry 'register reconnect-controller* controller-swiss-num)
                 #t))
              #:promise? #t))
        #:promise? #t)))

;; Now whats left to do is try and connect
(test-equal "Test prelay reconnects and remains reachable after connection breakage"
  #(ok "Hello testing, my name is Reconnect!")
  (resolve-vow-and-return-result
   test-vat
   (lambda ()
     (define-values (vat mycapn client-netlayer server-netlayer)
       (setup-prelay "testing"))
     ;; Have to make sure we've waited until it's halted before we reconnect.
     (define reconnected-greeter-vow
       (on reconnect-ready?
           (lambda _
             (<- mycapn 'enliven reconnect-greeter-sref))
           #:promise? #t))
     (<- reconnected-greeter-vow "testing"))))

(test-end "test-prelay")
