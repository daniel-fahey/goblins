;;; Copyright 2023-2024 Jessica Tallon
;;; Copyright 2020-2022 Christine Lemmer Webber
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

(use-modules (goblins)
             (goblins vat)
             (goblins actor-lib facet)
             (goblins actor-lib methods)
             (goblins actor-lib ward)
             (goblins actor-lib joiners)
             (goblins ghash)
             (goblins ocapn ids)
             (goblins ocapn captp)
             (goblins ocapn netlayer onion)
             (goblins ocapn netlayer tcp-tls)
             (goblins ocapn netlayer prelay)
             (goblins ocapn netlayer prelay-utils)
             (fibers conditions)
             (ice-9 match))

(define relay-vat
  (spawn-vat #:name "relay-vat"))

(define (spawn-netlayer-by-name name options)
  (match name
    ["onion" (new-onion-netlayer)]
    ["tcp-tls"
     (match options
       [(host port)
        (new-tcp-tls-netlayer host #:port (string->number port))]
       [(host)
        (new-tcp-tls-netlayer host)]
       [()
        (new-tcp-tls-netlayer "localhost")]
       [something-else
        (error "Expected arguments: tcp-tls <hostname> [<port>]")])]))

;; We need a condition to decide when we're able to quit (or rather stop
;; waiting), we shouldn't quit until we've finished doing what we need to which
;; often will rely on promises resolving. This condition is triggered by the
;; code below once we've finished doing what we need to.
(define can-quit?
  (make-condition))

(define (^register-facet _bcom mycapn netlayer-name)
  (match-lambda*
    [('register obj)
     (<- mycapn 'register obj netlayer-name)]))

(match (command-line)
  [(_cmd "new-relay" netlayer-name netlayer-options ...)
   (with-vat relay-vat
     (define base-netlayer
       (spawn-netlayer-by-name netlayer-name netlayer-options))
     (define base-mycapn
       (spawn-mycapn base-netlayer))
     (define prelay-admin
       (spawn ^prelay-admin
              (spawn ^facet base-mycapn 'enliven)
              (spawn ^register-facet base-mycapn ($ base-netlayer 'netlayer-name))))

     (on (<- base-mycapn 'register prelay-admin ($ base-netlayer 'netlayer-name))
         (lambda (relay-admin-sref)
           (format #t "New relay created successfully, the admin object is at: ~a\n"
                   (ocapn-id->string relay-admin-sref))))
     ;; NOTE: This has no quit condition as the relay wants to stay open until
     ;;       we shutdown.
     )]
  [(_cmd "add-account" relay-admin-sref-str account-name)
   (define relay-admin-sref
     (string->ocapn-id relay-admin-sref-str))
   (define relay-admin-node
     (ocapn-sturdyref-node relay-admin-sref))
   (with-vat relay-vat
     (define netlayer
       (spawn-netlayer-by-name (symbol->string (ocapn-node-transport relay-admin-node)) (list)))
     (define mycapn
       (spawn-mycapn netlayer))
     (define relay-admin-vow (<- mycapn 'enliven relay-admin-sref))
     (on (<- relay-admin-vow 'add-account account-name)
         (lambda (activation-sref)
           (format #t "Account created, provide this one-time use sturdyref to fetch it: ~a\n"
                   (ocapn-id->string activation-sref)))
         #:catch
         (lambda (err)
           (display "Oh no! Something went wrong (maybe check the logs for the relay server).\n"))
         #:finally
         (lambda ()
           (signal-condition! can-quit?))))]
  [(_cmd "list-accounts" relay-admin-sref-str)
   (define relay-admin-sref
     (string->ocapn-id relay-admin-sref-str))
   (define relay-admin-node
     (ocapn-sturdyref-node relay-admin-sref))
   (with-vat relay-vat
     (define netlayer
       (spawn-netlayer-by-name (symbol->string (ocapn-node-transport relay-admin-node)) (list)))
     (define mycapn
       (spawn-mycapn netlayer))
     (define relay-admin-vow (<- mycapn 'enliven relay-admin-sref))
     (on (<- relay-admin-vow 'get-accounts)
         (lambda (accounts)
           (if (zero? (length accounts))
               (begin
                 (display "No accounts exist yet!\n")
                 (signal-condition! can-quit?))
               (begin
                 (display "Accounts:\n")
                 (map (lambda (account)
                        (format #t "- ~a\n" account))
                      accounts))))
         #:finally
         (lambda ()
           (signal-condition! can-quit?))))]
  [unknown-cmd
   (let ((program-name (car unknown-cmd)))
     (format #t "Unknown command: ~a, please use one of the following:

~a new-relay <netlayer> [<netlayer-options> ...]              Sets up a new relay on specified netlayer.
~a add-account <relay-server-sturdyref> <account-name>        Adds a new account with given name.
~a list-accounts <relay-server-sturdyref>                     Lists all account names configured by this admin.

Currently supported netlayers:
- tcp-tls <hostname> [<port>]
- onion\n"
             unknown-cmd
             program-name program-name program-name))
   (signal-condition! can-quit?)])

(wait can-quit?)
