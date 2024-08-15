;;; Copyright 2023-2024 Jessica Tallon
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

(define-module (goblins ocapn netlayer prelay-utils)
  #:use-module (goblins)
  #:use-module (goblins ghash)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer prelay)
  #:use-module (goblins ocapn netlayer onion)
  #:use-module (goblins ocapn netlayer tcp-tls)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins actor-lib facet)
  #:use-module (fibers channels)
  #:use-module (ice-9 match)
  #:export (^prelay-admin
            fetch-and-spawn-prelay-netlayer
            prelay-utils-env))

(define-actor (^relay-account bcom enliven register #:key [setup? #f])
  (define (setup-beh)
    (error "Already setup"))
  (define (main-beh)
    (define-values (prelay-endpoint prelay-controller)
      (spawn-prelay-pair enliven))
    (bcom (^relay-account bcom enliven register #:setup? #t)
          (all-of
           (<- register 'register prelay-endpoint)
           (<- register 'register prelay-controller))))
  (if setup?
      setup-beh
      main-beh))

(define-actor (^prelay-admin bcom enliven register #:optional [accounts (make-ghash)])
  "Allows for creating new prelay netlayer accounts with a name

It has two methods, the first `add-account' takes a name and creates a relay account
for that name. It provides a sturdyref back which can be used by the client exactly
once to configure and setup the relay.

The second method is `get-accounts' which lists all the account names that have been
created on this prelay-admin."

  (methods
   [(add-account name)
    (when (ghash-has-key? accounts name)
      (error "Account with name already exists" name))
    (define new-account (spawn ^relay-account enliven register))
    (bcom (^prelay-admin bcom enliven register
                         (ghash-set accounts name new-account))
          (<- register 'register new-account))]
   [(get-accounts)
    (ghash-fold
     (lambda (name revoke account-list)
       (cons name account-list))
     (list)
     accounts)]))

(define* (fetch-and-spawn-prelay-netlayer account-setup-sref
                                         #:key
                                         [netlayer #f]
                                         [mycapn #f])
  "Retrieves account from account-setup-sref and provides prelay-netlayer"
  (define account-setup-node
    (ocapn-sturdyref-node account-setup-sref))
  (define base-netlayer
    (or netlayer
        (match (ocapn-node-transport account-setup-node)
          ('onion (spawn ^onion-netlayer))
          ('tcp-tls (spawn ^tcp-tls-netlayer "localhost")))))

  ;; While most OCapN connections normally would expect connections to many
  ;; different nodes and support for handoffs between those, this situation is a
  ;; bit different.  We're just looking for a connection between this node and
  ;; the prelay "server", this is what this mycapn object is that we're setting
  ;; up. We should not expect any shortening or connection to ourselves problems
  ;; with this setup.
  (define base-mycapn
    (or mycapn (spawn-mycapn base-netlayer)))

  ;; Enliven the "setup" sturdyref and then setup the prelay netlayer with that.
  (define account-setup-vow
    (<- base-mycapn 'enliven account-setup-sref))
  (define account-vow (<- account-setup-vow))
  (define prelay-endpoint-sref-vow
    (on account-vow
      (match-lambda
        ((prelay-endpoint-sref _prelay-controller-sref)
         prelay-endpoint-sref))
      #:promise? #t))
  (define prelay-controller-sref-vow
    (on account-vow
        (match-lambda
          ((_prelay-endpoint-sref prelay-controller-sref)
           prelay-controller-sref))
        #:promise? #t))
  (spawn ^prelay-netlayer
         (spawn ^facet base-mycapn 'enliven)
         prelay-endpoint-sref-vow
         prelay-controller-sref-vow))

(define prelay-utils-env
  (make-persistence-env
   `((((goblins ocapn netlayer prelay-utils) ^relay-account) ,^relay-account)
     (((goblins ocapn netlayer prelay-utils) ^relay-admin) ,^prelay-admin))))
