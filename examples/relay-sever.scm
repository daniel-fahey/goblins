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
             (goblins ocapn netlayer relay)
             (goblins ocapn netlayer relay-utils)
             (fibers conditions)
             (ice-9 match))

(define relay-vat
  (spawn-vat #:name "relay-vat"))

(define (valid-port? str-port)
  (define port
    (string->number str-port))
  (and (>= 1 port)
       (<= 65535 port)))

(define (spawn-netlayer-by-name name options)
  (match name
    ["onion" (new-onion-netlayer)]
    ["tcp-tls"
     (match options
       [(host (? valid-port? port))
        (new-tcp-tls-netlayer host #:port (string->number port))]
       [(host)
        (new-tcp-tls-netlayer host)]
       [something-else
        (error "Insufficient options provided for tcp-tls netlayer, expected at least host")])]))

(define can-quit?
  (make-condition))

(match (command-line)
  [(cmd "new-relay" netlayer-name netlayer-options ...)
   (with-vat relay-vat
     (define base-netlayer
       (spawn-netlayer-by-name netlayer-name netlayer-options))
     (define base-mycapn
       (spawn-mycapn base-netlayer))
     (define relay-admin
       (spawn ^relay-admin
              (lambda (sref) (<- base-mycapn 'enliven sref))
              (lambda (obj) (<- base-mycapn 'register obj ($ base-netlayer 'netlayer-name)))))

     (on (<- base-mycapn 'register relay-admin ($ base-netlayer 'netlayer-name))
         (lambda (relay-admin-sref)
           (format #t "New relay created successfully, the admin object is at: ~a\n"
                   (ocapn-id->string relay-admin-sref))))
     ;; NOTE: This has no quit condition as the relay wants to stay open until
     ;;       we shutdown.
     )]
  [(cmd "add-account" relay-admin-sref-str account-name)
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
  [(cmd "list-accounts" relay-admin-sref-str)
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
           (signal-condition! can-quit?))))])

(wait can-quit?)
