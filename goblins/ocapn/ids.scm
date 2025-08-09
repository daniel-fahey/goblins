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
  #:use-module (goblins utils hashmap)
  #:use-module (goblins contrib base64)
  #:use-module (goblins contrib syrup)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:export (<ocapn-peer>
            make-ocapn-peer
            ocapn-peer?
            ocapn-peer-transport
            ocapn-peer-designator
            ocapn-peer-hints
            ocapn-id->ocapn-peer
            marshall::ocapn-peer
            unmarshall::ocapn-peer

            <ocapn-sturdyref>
            ocapn-sturdyref
            ocapn-sturdyref?
            make-ocapn-sturdyref
            ocapn-sturdyref?
            ocapn-sturdyref-peer
            ocapn-sturdyref-swiss-num
            uri->ocapn-sturdyref
            marshall::ocapn-sturdyref
            unmarshall::ocapn-sturdyref

            ocapn-id?
            same-peer-location?
            ocapn-id->uri
            ocapn-id->string
            string->ocapn-id

            ;; Deprecated
            <ocapn-node>
            make-ocapn-node
            ocapn-node?
            ocapn-node-transport
            ocapn-node-designator
            ocapn-node-hints
            ocapn-id->ocapn-peer
            marshall::ocapn-node
            unmarshall::ocapn-node
            ocapn-sturdyref-node
            ocapn-id->ocapn-node
            ))

;; Ocapn peer type URI:
;;
;;   ocapn://<designator>.<transport>[?<transport-hints>]
;;
;;   <ocapn-peer $transport $transport-designator $transport-hints>
;;
;; . o O (Are hints really a good idea or needed anymore?)

;; EG: "ocapn://wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd.onion?foo=bar"
(define-syrup-record-type <ocapn-peer>
  (make-ocapn-peer transport designator hints)
  ocapn-peer?
  ocapn-peer marshall::ocapn-peer unmarshall::ocapn-peer
  (transport ocapn-peer-transport)
  (designator ocapn-peer-designator)
  (hints ocapn-peer-hints))

;; ocapn peers give the capability to access the peer, these shouldn't be
;; leaked in tracebacks.
(set-record-type-printer!
 <ocapn-peer>
 (lambda (peer port)
   (format port "#<ocapn-peer transport: ~a designator: *redacted*>"
           (ocapn-peer-transport peer))))

;; Ocapn swissnum URI:
;;
;;   ocapn://abpoiyaspodyoiapsdyiopbasyop.onion/s/3cbe8e02-ca27-4699-b2dd-3e284c71fa96?foo=bar
;;
;;   ocapn://<designator>.<transport>/s/<swiss-num>[?<transport-hints>]
;;
;;   <ocapn-sturdyref <ocapn-peer $transport $transport-designator $transport-hints>
;;                    $swiss-num>
(define-syrup-record-type <ocapn-sturdyref>
  (make-ocapn-sturdyref peer swiss-num)
  ocapn-sturdyref?
  ocapn-sturdyref marshall::ocapn-sturdyref unmarshall::ocapn-sturdyref
  (peer ocapn-sturdyref-peer)
  (swiss-num ocapn-sturdyref-swiss-num))

;; ocapn sturdyref give the capability to access the object, these shouldn't be
;; leaked in tracebacks.
(set-record-type-printer!
 <ocapn-sturdyref>
 (lambda (sturdyref port)
   (format port "#<ocapn-sturdyref peer: ~a swiss-num: *redacted*>"
           (ocapn-sturdyref-peer sturdyref))))

;; Ocapn certificate URI:
;;
;;   ocapn://<designator>.<transport>/c/<cert>
;;
;;   <ocapn-cert <ocapn-peer $transport $transport-designator $transport-hints>
;;               $cert>
;; (define-record-type <ocapn-cert>
;;   (make-ocapn-cert peer certdata)
;;   ocapn-cert?
;;   (peer ocapn-cert-peer)
;;   (certdata ocapn-cert-certdata))
;;

;; Ocapn bearer certificate union URI:
;;
;;   ocapn://<designator>.<transport>/b/<cert>/<key-type>.<private-key>
;;
;;   <ocapn-bearer-union <ocapn-cert <ocapn-peer $transport
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
  (or (ocapn-peer? obj)
      (ocapn-sturdyref? obj)))

(define (ocapn-id->ocapn-peer ocapn-id)
  (match ocapn-id
    [(? ocapn-peer?) ocapn-id]
    [($ <ocapn-sturdyref> ocapn-peer _sn) ocapn-peer]))

;; Checks for the equivalence between two ocapn-peers (including
;; ocapn-peers that are nested within other ocapn ID structs),
;; ignoring hints.
(define (same-peer-location? ocapn-id1 ocapn-id2)
  (define peer1 (ocapn-id->ocapn-peer ocapn-id1))
  (define peer2 (ocapn-id->ocapn-peer ocapn-id2))
  (match-let ((($ <ocapn-peer> p1-transport p1-designator _)
               peer1)
              (($ <ocapn-peer> p2-transport p2-designator _)
               peer2))
    (and (equal? p1-transport p2-transport)
         (equal? p1-designator p2-designator))))

(define (string->ocapn-id string-uri)
  (define (query->hints query)
    (and query
         ;; The URI query string is *not* pre-parsed into key/value
         ;; pairs, as the 'foo=1&bar=2' notation is just a convention.
         ;; So, we need to parse it ourselves.
         (fold
          (lambda (hint prev)
            (match (string-split hint #\=)
              ((key value) (hashmap-set prev key (uri-decode value)))))
          (make-hashmap)
          (string-split query #\&))))

  (define (uri->ocapn-peer uri)
    (let* ((host (uri-host uri))
           (final-part (string-rindex host #\.))
           (transport (string->symbol (substring host (+ 1 final-part))))
           (designator (substring host 0 final-part))
           (hints (query->hints (uri-query uri))))
      (make-ocapn-peer transport designator hints)))

  (define (uri->ocapn-sturdyref uri)
    (let ((path (string-trim (uri-path uri) #\/)))
      (make-ocapn-sturdyref
       (uri->ocapn-peer uri)
       (base64-decode (substring path (+ 1 (string-index path #\/)))
                      #:alphabet base64-url-alphabet
                      #:padding? #f))))

  (define (uri->ocapn-id uri)
    (let ((path (uri-path uri)))
      (cond [(or (string=? path "") (string=? path "/")) (uri->ocapn-peer uri)]
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
    (match hints
      [(? hashmap?)
       (let ((parts
              (hashmap-fold
               (lambda (key value prev)
                 (cons (format #f "~a=~a" key (uri-encode value)) prev))
               '()
                hints)))
         (string-join parts "&"))]
      [#f #f]))

  (unless (ocapn-id? ocapn-id)
    (error "Not a OCapN ID" ocapn-id))

  (match ocapn-id
    [($ <ocapn-peer> transport designator hints)
     (build-uri
      'ocapn
      #:host (string-join (list designator (symbol->string transport)) ".")
      #:query (hints->query hints))]

    [($ <ocapn-sturdyref> ($ <ocapn-peer> transport designator hints)
                          swiss-num)
     (build-uri
      'ocapn
      #:host (string-join (list designator (symbol->string transport)) ".")
      #:path (string-append "/s/" (base64-encode swiss-num
                                                 #:alphabet base64-url-alphabet
                                                 #:padding? #f))
      #:query (hints->query hints))]))

(define (ocapn-id->string ocapn-id)
  (uri->string (ocapn-id->uri ocapn-id)))

;; Deprecation
(define-syntax-rule (define-deprecated (old-name arg ...) new-name)
  (define* (old-name arg ...)
    (issue-deprecation-warning
     (format #f "~a is deprecated in favor of ~a."
             (symbol->string 'old-name)
             (symbol->string 'new-name)))
    (new-name arg ...)))

(define <ocapn-node> <ocapn-peer>)
(define-deprecated (make-ocapn-node transport designator hints) make-ocapn-peer)
(define-deprecated (ocapn-node? node) ocapn-peer?)
(define-deprecated (ocapn-node-transport peer) ocapn-peer-transport)
(define-deprecated (ocapn-node-designator peer) ocapn-peer-designator)
(define-deprecated (ocapn-node-hints peer) ocapn-peer-hints)
(define marshall::ocapn-node marshall::ocapn-peer)
(define unmarshall::ocapn-node unmarshall::ocapn-peer)
(define-deprecated (ocapn-sturdyref-node sref) ocapn-sturdyref-peer)
(define-deprecated (ocapn-id->ocapn-node ocapn-id) ocapn-id->ocapn-peer)
