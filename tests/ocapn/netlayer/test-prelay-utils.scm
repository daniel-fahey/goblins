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


(define-module (tests ocapn netlayer test-prelay-utils)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn netlayer fake)
  #:use-module (goblins ocapn netlayer prelay-utils)
  #:use-module (goblins actor-lib facet)
  #:use-module (goblins actor-lib joiners)
  #:use-module (tests utils)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(test-begin "test-prelay-utils")

(define fake-network-vat (spawn-vat #:name "interwebs, but fake"))
(define fake-network
  (with-vat fake-network-vat
    (spawn ^fake-network)))

(define prelay-server-vat
  (spawn-vat #:name "prelay server"))
(define prelay-server-fake-netlayer
  (with-vat prelay-server-vat
    (let ((new-conn-ch (make-channel)))
      (<-np fake-network 'register "prelay-server-fake" new-conn-ch)
      (spawn ^fake-netlayer
             "prelay-server-fake"
             fake-network
             new-conn-ch))))

(define prelay-admin
  (with-vat prelay-server-vat
    (define relay-mycapn
      (spawn-mycapn prelay-server-fake-netlayer))
    (spawn ^prelay-admin
           (spawn ^facet relay-mycapn 'enliven)
           (spawn (lambda _
                    (match-lambda*
                      (('register obj)
                      (<- relay-mycapn 'register obj 'fake))))))))

;; Create a user on the prelay
(define alice-account-activate-sref-vow
  (with-vat prelay-server-vat
    (<- prelay-admin 'add-account "alice")))
(define alice-account-activate-sref
  (match (resolve-vow-and-return-result
          prelay-server-vat
          (lambda () alice-account-activate-sref-vow))
    (#('ok sref) sref)))

(test-equal "Prelay admin get-accounts lists one account"
  #(ok ("alice"))
  (resolve-vow-and-return-result
   prelay-server-vat
   (lambda ()
     (<- prelay-admin 'get-accounts))))

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
(define alice-prelay-netlayer-vow
  (with-vat alice-vat
    (fetch-and-spawn-prelay-netlayer
     alice-account-activate-sref
     #:netlayer (spawn-fake-netlayer "alice"))))
(define alice-prelay-mycapn-vow
  (with-vat alice-vat
    (on alice-prelay-netlayer-vow
        spawn-mycapn
        #:promise? #t)))
(define alice-greeter-sref-vow
  (with-vat alice-vat
    (<- alice-prelay-mycapn-vow 'register (spawn ^greeter "Alice") 'prelay)))

;; Setup bob's end
(define bob-account-activate-sref-vow
  (with-vat prelay-server-vat
    (<- prelay-admin 'add-account "bob")))
(define bob-account-activate-sref
  (match (resolve-vow-and-return-result
          prelay-server-vat
          (lambda () bob-account-activate-sref-vow))
    (#('ok sref) sref)))

(test-assert "Prelay admin get-accounts lists both accounts"
  (match (resolve-vow-and-return-result
          prelay-server-vat
          (lambda ()
            (<- prelay-admin 'get-accounts)))
    [#(ok accounts)
     (equal? '("alice" "bob") (sort accounts string<=?))]
    [_ #f]))

(define bob-vat
  (spawn-vat #:name "bob"))
(define bob-prelay-netlayer-vow
  (with-vat bob-vat
    (fetch-and-spawn-prelay-netlayer
     bob-account-activate-sref
     #:netlayer (spawn-fake-netlayer "bob"))))
(define bob-prelay-mycapn-vow
  (with-vat bob-vat
    (on bob-prelay-netlayer-vow
        spawn-mycapn
        #:promise? #t)))
(define alice-greeter-on-bob-vow
  (with-vat bob-vat
    (on alice-greeter-sref-vow
        (lambda (alice-greeter-sref)
          (<- bob-prelay-mycapn-vow 'enliven alice-greeter-sref))
        #:promise? #t)))
(test-equal "Able to send message across after setup"
  #(ok "Hello Bob, my name is Alice!")
  (resolve-vow-and-return-result
   bob-vat
   (lambda ()
     (<- alice-greeter-on-bob-vow "Bob"))))

(test-end "test-prelay-utils")
