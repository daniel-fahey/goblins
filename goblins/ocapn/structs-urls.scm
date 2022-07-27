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
            marshall::ocapn-machine
            unmarshall::ocapn-machine

            make-ocapn-sturdyref
            ocapn-sturdyref?
            ocapn-sturdyref-machine
            ocapn-sturdyref-swiss-num
            marshall::ocapn-sturdyref
            unmarshall::ocapn-sturdyref

            make-ocapn-cert
            ocapn-cert?
            ocapn-cert-machine
            ocapn-cert-certdata
            marshall::ocapn-cert
            unmarshall::ocapn-cert

            make-ocapn-bearer-union
            ocapn-bearer-union?
            ocapn-bearer-union-cert
            ocapn-bearer-union-key-type
            ocapn-bearer-union-private-key
            marshall::ocapn-bearer-union
            unmarshall::ocapn-bearer-union))

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
