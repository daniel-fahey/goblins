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

(define-module (goblins ocapn ids)
  #:use-module (goblins utils crypto)
  #:use-module (goblins contrib syrup)
  #:use-module (web uri)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:export (<ocapn-node>
            make-ocapn-node
            ocapn-node?
            ocapn-node-transport
            ocapn-node-designator
            ocapn-node-hints
            ocapn-id->ocapn-node
            marshall::ocapn-node
            unmarshall::ocapn-node

            <ocapn-sturdyref>
            ocapn-sturdyref
            ocapn-sturdyref?
            make-ocapn-sturdyref
            ocapn-sturdyref?
            ocapn-sturdyref-node
            ocapn-sturdyref-swiss-num
            uri->ocapn-sturdyref
            marshall::ocapn-sturdyref
            unmarshall::ocapn-sturdyref

            ocapn-id?
            same-node-location?
            ocapn-id->uri
            ocapn-id->string
            string->ocapn-id))

;; Ocapn node type URI:
;;
;;   ocapn://<designator>.<transport>[?<transport-hints>]
;;
;;   <ocapn-node $transport $transport-designator $transport-hints>
;;
;; . o O (Are hints really a good idea or needed anymore?)

;; EG: "ocapn://wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd.onion?foo=bar"
(define-syrup-record-type <ocapn-node>
  (make-ocapn-node transport designator hints)
  ocapn-node?
  ocapn-node marshall::ocapn-node unmarshall::ocapn-node
  (transport ocapn-node-transport)
  (designator ocapn-node-designator)
  (hints ocapn-node-hints))

;; ocapn nodes give the capability to access the node, these shouldn't be
;; leaked in tracebacks.
(set-record-type-printer!
 <ocapn-node>
 (lambda (node port)
   (format port "#<ocapn-node transport: ~a designator: *redacted*>"
           (ocapn-node-transport node))))

;; Ocapn swissnum URI:
;;
;;   ocapn://abpoiyaspodyoiapsdyiopbasyop.onion/s/3cbe8e02-ca27-4699-b2dd-3e284c71fa96?foo=bar
;;
;;   ocapn://<designator>.<transport>/s/<swiss-num>[?<transport-hints>]
;;
;;   <ocapn-sturdyref <ocapn-node $transport $transport-designator $transport-hints>
;;                    $swiss-num>
(define-syrup-record-type <ocapn-sturdyref>
  (make-ocapn-sturdyref node swiss-num)
  ocapn-sturdyref?
  ocapn-sturdyref marshall::ocapn-sturdyref unmarshall::ocapn-sturdyref
  (node ocapn-sturdyref-node)
  (swiss-num ocapn-sturdyref-swiss-num))

;; ocapn sturdyref give the capability to access the object, these shouldn't be
;; leaked in tracebacks.
(set-record-type-printer!
 <ocapn-sturdyref>
 (lambda (sturdyref port)
   (format port "#<ocapn-sturdyref node: ~a swiss-num: *redacted*>"
           (ocapn-sturdyref-node sturdyref))))

;; Ocapn certificate URI:
;;
;;   ocapn://<designator>.<transport>/c/<cert>
;;
;;   <ocapn-cert <ocapn-node $transport $transport-designator $transport-hints>
;;               $cert>
;; (define-record-type <ocapn-cert>
;;   (make-ocapn-cert node certdata)
;;   ocapn-cert?
;;   (node ocapn-cert-node)
;;   (certdata ocapn-cert-certdata))
;;

;; Ocapn bearer certificate union URI:
;;
;;   ocapn://<designator>.<transport>/b/<cert>/<key-type>.<private-key>
;;
;;   <ocapn-bearer-union <ocapn-cert <ocapn-node $transport
;;                                                  $transport-designator
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

(define (ocapn-id? obj)
  (or (ocapn-node? obj)
      (ocapn-sturdyref? obj)))

(define (ocapn-id->ocapn-node ocapn-id)
  (match ocapn-id
    [(? ocapn-node?) ocapn-id]
    [($ <ocapn-sturdyref> ocapn-node _sn) ocapn-node]))

;; Checks for the equivalence between two ocapn-nodes (including
;; ocapn-nodes that are nested within other ocapn ID structs),
;; ignoring hints.
(define (same-node-location? ocapn-id1 ocapn-id2)
  (define node1 (ocapn-id->ocapn-node ocapn-id1))
  (define node2 (ocapn-id->ocapn-node ocapn-id2))
  (match-let ((($ <ocapn-node> m1-transport m1-designator _m1-hints)
               node1)
              (($ <ocapn-node> m2-transport m2-designator _m2-hints)
               node2))
    (and (equal? m1-transport m2-transport)
         (equal? m1-designator m2-designator))))

(define (string->ocapn-id string-uri)
  (define (query->hints query)
    (and query
         ;; The URI query string is *not* pre-parsed into key/value
         ;; pairs, as the 'foo=1&bar=2' notation is just a convention.
         ;; So, we need to parse it ourselves.
         (map (lambda (hint)
                (match (string-split hint #\=)
                  ((key value)
                   ;; Hints are lists of 2 elements, *not pairs*,
                   ;; because Syrup can serialize proper lists but not
                   ;; pairs.
                   (list (string->symbol (uri-decode key))
                         (uri-decode value)))))
              (string-split query #\&))))

  (define (uri->ocapn-node uri)
    (let* ((host (uri-host uri))
           (final-part (string-rindex host #\.))
           (transport (string->symbol (substring host (+ 1 final-part))))
           (designator (substring host 0 final-part))
           (hints (query->hints (uri-query uri))))
      (make-ocapn-node transport designator hints)))

  (define (uri->ocapn-sturdyref uri)
    (let ((path (string-trim (uri-path uri) #\/)))
      (make-ocapn-sturdyref
       (uri->ocapn-node uri)
       (url-base64-decode (substring path (+ 1 (string-index path #\/)))))))

  (define (uri->ocapn-id uri)
    (let ((path (uri-path uri)))
      (cond [(or (string=? path "") (string=? path "/")) (uri->ocapn-node uri)]
            [(string-prefix? "/s/" path) (uri->ocapn-sturdyref uri)]
            [#t (error "Unknown ocapn URI type" uri)])))

  (unless (string? string-uri)
    (error "Not a valid OCapN URI:" string-uri))

  (let ((uri (string->uri string-uri)))
    (unless (and uri (eq? (uri-scheme uri) 'ocapn))
      (error "Not a valid OCapN URI:" string-uri))
    (uri->ocapn-id uri)))

(define (ocapn-id->uri ocapn-id)
  (define (hints->query hints)
    (and hints
         (string-join (map (match-lambda
                             (((? symbol? key) (? string? value))
                              (string-append (uri-encode (symbol->string key))
                                             "="
                                             (uri-encode value))))
                           hints)
                      "&")))

  (unless (ocapn-id? ocapn-id)
    (error "Not a OCapN ID" ocapn-id))

  (match ocapn-id
    [($ <ocapn-node> transport designator hints)
     (build-uri
      'ocapn
      #:host (string-join (list designator (symbol->string transport)) ".")
      #:query (hints->query hints))]

    [($ <ocapn-sturdyref> ($ <ocapn-node> transport designator hints)
                          swiss-num)
     (build-uri
      'ocapn
      #:host (string-join (list designator (symbol->string transport)) ".")
      #:path (string-append "/s/" (url-base64-encode swiss-num))
      #:query (hints->query hints))]))

(define (ocapn-id->string ocapn-id)
  (uri->string (ocapn-id->uri ocapn-id)))
