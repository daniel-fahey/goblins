;;; Copyright 2021 Christine Lemmer-Webber
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

(define-module (goblins ocapn structs-urls)
  #:use-module (goblins ocapn marshalling)
  #:use-module (goblins utils crypto-stuff)
  #:use-module (srfi srfi-9)
  #:use-module (web uri)
  #:use-module (ice-9 match)
  #:export (<ocapn-machine>
            make-ocapn-machine
            ocapn-machine?
            ocapn-machine-transport
            ocapn-machine-address
            ocapn-machine-hints
            ocapn-struct?
            ocapn-struct->ocapn-machine
            same-machine-location?
            marshall::ocapn-machine
            unmarshall::ocapn-machine

            <ocapn-sturdyref>
            ocapn-sturdyref
            ocapn-sturdyref?
            make-ocapn-sturdyref
            ocapn-sturdyref?
            ocapn-sturdyref-machine
            ocapn-sturdyref-swiss-num
            uri->ocapn-sturdyref
            marshall::ocapn-sturdyref
            unmarshall::ocapn-sturdyref

            ocapn-uri->string
            string->ocapn-uri))

;; Ocapn machine type URI:
;;
;;   ocapn://m.<transport>.<transport-address>[.<transport-hints>]
;;
;;   <ocapn-machine $transport $transport-address $transport-hints>
;;
;; . o O (Are hints really a good idea or needed anymore?)

;; EG: "ocapn://m.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
(define-record-type <ocapn-machine>
  (make-ocapn-machine transport address hints)
  ocapn-machine?
  (transport ocapn-machine-transport)
  (address ocapn-machine-address)
  (hints ocapn-machine-hints))

(define-values (marshall::ocapn-machine unmarshall::ocapn-machine)
  (make-marshallers <ocapn-machine> #:name 'ocapn-machine))

;; Ocapn swissnum URI:
;;
;;   ocapn://s.onion.abpoiyaspodyoiapsdyiopbasyop/3cbe8e02-ca27-4699-b2dd-3e284c71fa96
;;
;;   ocapn://s.<transport>.<transport-address>/<swiss-num>
;;
;;   <ocapn-sturdyref <ocapn-machine $transport $transport-address $transport-hints>
;;                    $swiss-num>
(define-record-type <ocapn-sturdyref>
  (make-ocapn-sturdyref machine swiss-num)
  ocapn-sturdyref?
  (machine ocapn-sturdyref-machine)
  (swiss-num ocapn-sturdyref-swiss-num))

(define-values (marshall::ocapn-sturdyref unmarshall::ocapn-sturdyref)
  (make-marshallers <ocapn-sturdyref> #:name 'ocapn-sturdyref))

;; Ocapn certificate URI:
;;
;;   ocapn://c.<transport>.<transport-address>/<cert>
;;
;;   <ocapn-cert <ocapn-machine $transport $transport-address $transport-hints>
;;               $cert>
;; (define-record-type <ocapn-cert>
;;   (make-ocapn-cert machine certdata)
;;   ocapn-cert?
;;   (machine ocapn-cert-machine)
;;   (certdata ocapn-cert-certdata))
;;
;; (define-values (marshall::ocapn-cert unmarshall::ocapn-cert)
;;   (make-marshallers <ocapn-cert> #:name 'ocapn-cert))

;; Ocapn bearer certificate union URI:
;;
;;   ocapn://b.<transport>.<transport-address>/<cert>/<key-type>.<private-key>
;;
;;   <ocapn-bearer-union <ocapn-cert <ocapn-machine $transport
;;                                                  $transport-address
;;                                                  $transport-hints>
;;                                   $cert>
;;                       $key-type
;;                       $private-key>
;; (define-record-type <ocapn-bearer-union>
;;   (make-ocapn-bearer-union cert key-type private-key)
;;   ocapn-bearer-union?
;;   (cert ocapn-bearer-union-cert)
;;   (key-type ocapn-bearer-union-key-type)
;;   (private-key ocapn-bearer-union-private-key))
;;
;; (define-values (marshall::ocapn-bearer-union unmarshall::ocapn-bearer-union)
;;   (make-marshallers <ocapn-bearer-union> #:name 'ocapn-bearer-union))

(define (ocapn-uri? obj)
  (or (ocapn-machine? obj)
      (ocapn-sturdyref? obj)))

(define (ocapn-struct->ocapn-machine ocapn-struct)
  (match ocapn-struct
    [(? ocapn-machine?) ocapn-struct]
    [($ <ocapn-sturdyref> ocapn-machine _sn) ocapn-machine]))

;; Checks for the equivalence between two ocapn-machine structs
;; (including ocapn-machines nested in other ocapn-structs),
;; ignoring hints
(define (same-machine-location? ocapn-struct1 ocapn-struct2)
  (define machine1 (ocapn-struct->ocapn-machine ocapn-struct1))
  (define machine2 (ocapn-struct->ocapn-machine ocapn-struct2))
  (match-let ((($ <ocapn-machine> m1-transport m1-address _m1-hints)
               machine1)
              (($ <ocapn-machine> m2-transport m2-address _m2-hints)
               machine2))
    (and (equal? m1-transport m2-transport)
         (equal? m1-address m2-address))))

(define (string->ocapn-uri string-uri)
  (define (host->ocapn-machine host)
    (let* ((first-delimiter-position (string-contains host "."))
           (transport (substring host 0 first-delimiter-position))
           (address (substring host (+ 1 first-delimiter-position))))
      ;; TODO: support hints?
      (make-ocapn-machine (string->symbol transport) address #f)))

  (define (host->ocapn-sturdyref host uri)
    (make-ocapn-sturdyref
     (host->ocapn-machine host)
     (url-base64-decode (string-trim (uri-path uri) #\/))))

  (define (host->ocapn-struct uri)
    (let* ((host (uri-host uri))
           (first-delimiter-position (string-contains host "."))
           (uri-type (substring host 0 first-delimiter-position))
           (rest-of-host (substring host (+ 1 first-delimiter-position))))
      (match uri-type
        ["m" (host->ocapn-machine rest-of-host)]
        ["s" (host->ocapn-sturdyref rest-of-host uri)])))

  (unless (string? string-uri)
    (error "Not a valid OCapN uri:" string-uri))

  (let ((uri (string->uri string-uri)))
    (unless (eq? (uri-scheme uri) 'ocapn)
      (error "Not a valid OCapN uri:" string-uri))
    (host->ocapn-struct uri)))

(define (ocapn-uri->string ocapn-uri)
  (unless (ocapn-uri? ocapn-uri)
    (error "Not a OCapN uri" ocapn-uri))

  (define string-uri
    (match ocapn-uri
      [($ <ocapn-machine> transport address _hints)
       (string-join (list "m" (symbol->string transport) address) ".")]
      [($ <ocapn-sturdyref> ($ <ocapn-machine> transport address _hints)
          swiss-num)
       (string-append
        (string-join (list "s" (symbol->string transport) address) ".")
        "/"
        (url-base64-encode swiss-num))]))
  (string-concatenate (list "ocapn://" string-uri)))
