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

(define-module (goblins ocapn netlayer relay-utils)
  #:use-module (goblins)
  #:use-module (goblins ghash)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer relay)
  #:use-module (goblins ocapn netlayer onion)
  #:use-module (goblins ocapn netlayer tcp-tls)
  #:use-module (goblins ocapn netlayer fake)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib joiners)
  #:use-module (fibers channels)
  #:use-module (ice-9 match)
  #:export (^relay-admin
            fetch-and-spawn-relay-netlayer))

(define* (^relay-admin bcom enliven register #:optional [accounts (make-ghash)])
  (define (^relay-account bcom)
    (lambda ()
      (define-values (relay-endpoint relay-controller)
        (spawn-relay-pair enliven))

      (define (already-setup-beh)
        (error "Already setup"))

      (bcom already-setup-beh
            (all-of
             (register relay-endpoint)
             relay-controller))))

  (methods
   [(add-account name)
    (when (ghash-has-key? accounts name)
      (error "Account with name already exists" name))
    (define new-account (spawn ^relay-account))
    (bcom (^relay-admin bcom enliven register
                        (ghash-set accounts name new-account))
          (register new-account))]
   [(get-accounts)
    (ghash-fold
     (lambda (name revoke account-list)
       (cons name account-list))
     (list)
     accounts)]))

(define* (fetch-and-spawn-relay-netlayer account-setup-sref
                                         #:key
                                         (tcp-tls-hostname "localhost")
                                         (fake-network #f)
                                         (additional-netlayers '()))
  "Retrieves account from account-setup-sref and provides relay-netlayer"
  ;; TODO: Replace me when we have a better way of setting up a netlayer
  ;;       automatically so we don't need to do this.
  (define account-setup-node
    (ocapn-sturdyref-node account-setup-sref))
  (define account-netlayer
    (match (ocapn-node-transport account-setup-node)
      ('onion (new-onion-netlayer))
      ('tcp-tls (new-tcp-tls-netlayer "localhost"))
      ('fake (spawn ^fake-netlayer "fake-network" fake-network (make-channel)))))
  (define account-netlayer-mycapn
    (apply spawn-mycapn account-netlayer additional-netlayers))
  (define account-setup-vow
    (<- account-netlayer-mycapn 'enliven account-setup-sref))
  (on (<- account-setup-vow)
      (match-lambda
        ((relay-endpoint-sref relay-controller) 
         (spawn ^relay-netlayer
                (lambda (sref) (<- account-netlayer-mycapn 'enliven sref))
                relay-endpoint-sref
                relay-controller)))
      #:promise? #t))
