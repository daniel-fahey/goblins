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

;; TODO: Kinda misnamed just because copied straight from the racket code
(define-module (goblins ocapn structs-urls)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-9)
  #:use-module (goblins ocapn marshalling)
  #:export (<ocapn-machine>
            make-ocapn-machine
            ocapn-machine?
            ocapn-machine-transport
            ocapn-machine-address
            ocapn-machine-hints
            ocapn-struct?
            ocapn-struct->ocapn-machine
            uri->ocapn-machine
            uri->ocapn-sturdyref
            uri->ocapn-cert
            uri->ocapn-bearer-union
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
            marshall::ocapn-sturdyref
            unmarshall::ocapn-sturdyref


            <ocapn-cert>
            ocapn-cert
            ocapn-cert?
            make-ocapn-cert
            ocapn-cert?
            ocapn-cert-machine
            ocapn-cert-certdata
            marshall::ocapn-cert
            unmarshall::ocapn-cert

            <ocapn-bearer-union>
            ocapn-bearer-union

            ocapn-bearer-union?
            ocapn-bearer-union-cert
            ocapn-bearer-union-key-type
            ocapn-bearer-union-private-key
            marshall::ocapn-bearer-union
            unmarshall::ocapn-bearer-union

            same-machine-location?))

;; Ocapn machine type URI:
;;
;;   ocapn:m.<transport>.<transport-address>[.<transport-hints>]
;;
;;   <ocapn-machine $transport $transport-address $transport-hints>
;;
;; . o O (Are hints really a good idea or needed anymore?)

;; EG: "ocapn:m.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
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
;;   ocapn:s.onion.abpoiyaspodyoiapsdyiopbasyop/3cbe8e02-ca27-4699-b2dd-3e284c71fa96
;;
;;   ocapn:s.<transport>.<transport-address>/<swiss-num>
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
;;   ocapn:c.<transport>.<transport-address>/<cert>
;;
;;   <ocapn-cert <ocapn-machine $transport $transport-address $transport-hints>
;;               $cert>
(define-record-type <ocapn-cert>
  (make-ocapn-cert machine certdata)
  ocapn-cert?
  (machine ocapn-cert-machine)
  (certdata ocapn-cert-certdata))

(define-values (marshall::ocapn-cert unmarshall::ocapn-cert)
  (make-marshallers <ocapn-cert> #:name 'ocapn-cert))

;; Ocapn bearer certificate union URI:
;;
;;   ocapn:b.<transport>.<transport-address>/<cert>/<key-type>.<private-key>
;;
;;   <ocapn-bearer-union <ocapn-cert <ocapn-machine $transport
;;                                                  $transport-address
;;                                                  $transport-hints>
;;                                   $cert>
;;                       $key-type
;;                       $private-key>
(define-record-type <ocapn-bearer-union>
  (make-ocapn-bearer-union cert key-type private-key)
  ocapn-bearer-union?
  (cert ocapn-bearer-union-cert)
  (key-type ocapn-bearer-union-key-type)
  (private-key ocapn-bearer-union-private-key))

(define-values (marshall::ocapn-bearer-union unmarshall::ocapn-bearer-union)
  (make-marshallers <ocapn-bearer-union> #:name 'ocapn-bearer-union))

;; Some of these are commented out as they're not supported, however
;; we want to support them in future.
(define (ocapn-struct? obj)
  (or (ocapn-machine? obj)
      (ocapn-sturdyref? obj)
      (ocapn-bearer-union? obj)
      (ocapn-cert? obj)))

(define (ocapn-struct->ocapn-machine ocapn-struct)
  (match ocapn-struct
    [(? ocapn-machine?) ocapn-struct]
    [($ <ocapn-sturdyref> ocapn-machine _sn) ocapn-machine]
    [($ <ocapn-cert> ocapn-machine _cert) ocapn-machine]
    [($ <ocapn-bearer-union> ($ <ocapn-cert> ocapn-machine _cert) _key-type _private-key)
     ocapn-machine]))

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

(define-record-type <ocapn-uri>
  (make-ocapn-uri type transport address path)
  ocapn-uri?
  (type ocapn-uri-type)
  (transport ocapn-uri-transport)
  (address ocapn-uri-address)
  (path ocapn-uri-path))

(define string->ocapn-uri
  (let* ((type-pat "[A-z0-9]+")
   (transport-pat "[A-z0-9]+")
   (address-pat "[A-z0-9\\.]+")
   (path-pat ".*")
   (uri-pat
    (format #f "ocapn:(~a)\\.(~a)\\.(~a)[\\/]?(~a)"
      type-pat transport-pat address-pat path-pat))
   (uri-regex (make-regexp uri-pat)))
    (lambda (uri)
      (let ((m (regexp-exec uri-regex uri)))
  (make-ocapn-uri
   (string->symbol (match:substring m 1))
   (string->symbol (match:substring m 2))
   (match:substring m 3)
   (match:substring m 4))))))

(define (ocapn-uri->string uri)
  (define machine-address
    (string-append
     "ocapn:"
     (symbol->string (ocapn-uri-type uri))
     "."
     (ocapn-uri-transport uri)
     "."
     (ocapn-uri-address uri)))
  (if (eq? (ocapn-uri-type uri) 'm)
      machine-address
      (string-append "/" (ocapn-uri-path uri))))


(define (uri->ocapn-machine machine-address)
  (define uri
    (match machine-address
      ((? string?) (string->ocapn-uri machine-address))
      ((? ocapn-uri?) machine-address)
      (_ (error "Unsupported type"))))
  (make-ocapn-machine
   (ocapn-uri-transport uri)
   (ocapn-uri-address uri)
   #f))

(define (uri->ocapn-sturdyref studyref)
  (let ((uri (string->ocapn-uri studyref)))
    (when (string-null? (ocapn-uri-path uri))
      (error (format #f "No swiss-num given for uri (~a)"
         (ocapn-uri->string uri))))
    (make-ocapn-sturdyref
     (uri->ocapn-machine uri)
     (ocapn-uri-path uri))))

(define (uri->ocapn-cert cert)
  (let ((uri (string->ocapn-uri cert)))
    (when (string-null? (ocapn-uri-path uri))
      (error (format #f "No cert given for uri (~a)"
         (ocapn-uri->string uri))))
    (make-ocapn-cert
     (uri->ocapn-machine uri)
     (ocapn-uri-path uri))))

(define (uri->ocapn-bearer-union bearer-union)
  (let ((uri (string->ocapn-uri bearer-union)))
    (define union-parts
      (string-split (ocapn-uri-path uri) #\/))
    (define cert
      (car union-parts))
    (define-values (key-type private-key)
      (let ((parts (string-split (list-ref union-parts 1) #\.)))
  (values (list-ref parts 0)
    (list-ref parts 1))))

    (make-ocapn-bearer-union
     (ocapn-cert (uri->ocapn-machine uri) cert)
     (string->symbol key-type)
     private-key)))
