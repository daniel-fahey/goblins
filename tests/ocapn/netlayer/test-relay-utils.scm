;;; Copyright 2023 Jessica Tallon
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


(define-module (tests ocapn netlayer test-relay-utils)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn netlayer fake)
  #:use-module (goblins ocapn netlayer relay-utils)
  #:use-module (goblins actor-lib facet)
  #:use-module (goblins actor-lib joiners)
  #:use-module (tests utils)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(test-begin "test-relay-utils")

(define fake-network-vat (spawn-vat #:name "interwebs, but fake"))
(define fake-network
  (with-vat fake-network-vat
    (spawn ^fake-network)))

(define relay-server-vat
  (spawn-vat #:name "relay server"))
(define relay-server-fake-netlayer
  (with-vat relay-server-vat
    (let ((new-conn-ch (make-channel)))
      (<-np fake-network 'register "relay-server-fake" new-conn-ch)
      (spawn ^fake-netlayer
             "relay-server-fake"
             fake-network
             new-conn-ch))))

(define relay-admin
  (with-vat relay-server-vat
    (define relay-mycapn
      (spawn-mycapn relay-server-fake-netlayer))
    (spawn ^relay-admin
           (spawn ^facet relay-mycapn 'enliven)
           (spawn (lambda _
                    (lambda (obj)
                      (<- relay-mycapn 'register obj 'fake)))))))

;; Create a user on the relay
(define alice-account-activate-sref-vow
  (with-vat relay-server-vat
    (<- relay-admin 'add-account "alice")))

(test-equal "Relay admin get-accounts lists one account"
  (resolve-vow-and-return-result
   relay-server-vat
   (lambda ()
     (<- relay-admin 'get-accounts)))
  #(ok ("alice")))

(define (^greeter _bcom my-name)
  (lambda (your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name)))

(define (spawn-fake-netlayer name)
  (let* ((new-conn-ch (make-channel))
         (netlayer (spawn ^fake-netlayer name fake-network new-conn-ch)))
    (<-np fake-network 'register name new-conn-ch)
    netlayer))

;; Setup alice's end
(define alice-vat
  (spawn-vat #:name "alice"))
(define alice-relay-netlayer-vow
  (with-vat alice-vat
    (on alice-account-activate-sref-vow
        (lambda (alice-account-activate-sref)
          (fetch-and-spawn-relay-netlayer
           alice-account-activate-sref
           #:netlayer (spawn-fake-netlayer "alice")))
        #:promise? #t)))
(define alice-relay-mycapn-vow
  (with-vat alice-vat
    (on alice-relay-netlayer-vow
        spawn-mycapn
        #:promise? #t)))
(define alice-greeter-sref-vow
  (with-vat alice-vat
    (<- alice-relay-mycapn-vow 'register (spawn ^greeter "Alice") 'relay)))

;; Setup bob's end
(define bob-account-activate-sref-vow
  (with-vat relay-server-vat
    (<- relay-admin 'add-account "bob")))

(test-assert "Relay admin get-accounts lists both accounts"
  (match (resolve-vow-and-return-result
          relay-server-vat
          (lambda ()
            (<- relay-admin 'get-accounts)))
  [#(ok accounts)
   (equal? '("alice" "bob") (sort accounts string<=?))]
  [_ #f]))

(define bob-vat
  (spawn-vat #:name "bob"))
(define bob-relay-netlayer-vow
  (with-vat bob-vat
    (on bob-account-activate-sref-vow
        (lambda (bob-account-sref)
          (fetch-and-spawn-relay-netlayer
           bob-account-sref
           #:netlayer (spawn-fake-netlayer "bob")))
        #:promise? #t)))
(define bob-relay-mycapn-vow
  (with-vat bob-vat
    (on bob-relay-netlayer-vow
        spawn-mycapn
        #:promise? #t)))
(define alice-greeter-on-bob-vow
  (with-vat bob-vat
    (on alice-greeter-sref-vow
        (lambda (alice-greeter-sref)
          (<- bob-relay-mycapn-vow 'enliven alice-greeter-sref))
        #:promise? #t)))
(test-equal "Able to send message across after setup"
  (resolve-vow-and-return-result
   bob-vat
   (lambda ()
     (<- alice-greeter-on-bob-vow "Bob")))
  #(ok "Hello Bob, my name is Alice!"))

(test-end "test-relay-utils")

