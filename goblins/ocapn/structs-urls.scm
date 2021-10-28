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
  #:use-module (goblins ocapn define-recordable)
  #:export (ocapn-machine
            make-ocapn-machine
            ocapn-machine?
            ocapn-machine-transport
            ocapn-machine-address
            ocapn-machine-hints
            marshall::ocapn-machine
            unmarshall::ocapn-machine

            ocapn-sturdyref
            ocapn-sturdyref?
            make-ocapn-sturdyref
            ocapn-sturdyref-machine
            ocapn-sturdyref-swiss-num
            marshall::ocapn-sturdyref
            unmarshall::ocapn-sturdyref

            ocapn-cert
            ocapn-cert?
            make-ocapn-cert
            ocapn-cert-machine
            ocapn-cert-certdata
            marshall::ocapn-cert
            unmarshall::ocapn-cert

            ocapn-bearer-union
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

(define-recordable ocapn-machine
  (transport address hints))

;; Ocapn swissnum URI:
;;
;;   ocapn:s.onion.abpoiyaspodyoiapsdyiopbasyop/3cbe8e02-ca27-4699-b2dd-3e284c71fa96
;;
;;   ocapn:s.<transport>.<transport-address>/<swiss-num>
;;
;;   <ocapn-sturdyref <ocapn-machine $transport $transport-address $transport-hints>
;;                    $swiss-num>

(define-recordable ocapn-sturdyref
  (machine swiss-num))

;; Ocapn certificate URI:
;;
;;   ocapn:c.<transport>.<transport-address>/<cert>
;;
;;   <ocapn-cert <ocapn-machine $transport $transport-address $transport-hints>
;;               $cert>

(define-recordable ocapn-cert
  (machine certdata))

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

(define-recordable ocapn-bearer-union
  (cert key-type private-key))


;; (define (ocapn-struct? obj)
;;   (or (ocapn-machine? obj)
;;       (ocapn-sturdyref? obj)
;;       (ocapn-cert? obj)
;;       (ocapn-bearer-union? obj)))

;; (define (ocapn-struct->ocapn-machine ocapn-struct)
;;   (match ocapn-struct
;;     [(? ocapn-machine?) ocapn-struct]
;;     [($ ocapn-sturdyref ocapn-machine _sn) ocapn-machine]
;;     [($ ocapn-cert ocapn-machine _cert) ocapn-machine]
;;     [($ ocapn-bearer-union ($ ocapn-cert ocapn-machine _cert) _key-type _private-key)
;;      ocapn-machine]))

;; ;; Checks for the equivalence between two ocapn-machine structs
;; ;; (including ocapn-machines nested in other ocapn-structs),
;; ;; ignoring hints
;; (define (same-machine-location? ocapn-struct1 ocapn-struct2)
;;   (define machine1 (ocapn-struct->ocapn-machine ocapn-struct1))
;;   (define machine2 (ocapn-struct->ocapn-machine ocapn-struct2))
;;   (match-let ([($ ocapn-machine m1-transport m1-address _m1-hints)
;;                machine1]
;;               [($ ocapn-machine m2-transport m2-address _m2-hints)
;;                machine2])
;;     (and (equal? m1-transport m2-transport)
;;          (equal? m1-address m2-address))))


;; (define (ocapn-url-pieces ocapn-url)
;;   (match ocapn-url
;;     [(url "ocapn" #f #f #f #f (list (path/param type-address-str '())
;;                                     rest-path-params ...)
;;           '() #f)
;;      (define rest-paths (map path/param-path rest-path-params))
;;      (define-values (ocapn-url-type netlayer-id address-str hints-str)
;;        (match (string-split type-address-str ".")
;;          [(list ocapn-url-type netlayer-id address-str hints-str)
;;           (values ocapn-url-type
;;                   (string->symbol netlayer-id)
;;                   address-str hints-str)]
;;          [(list ocapn-url-type netlayer-id address-str)
;;           (values ocapn-url-type
;;                   (string->symbol netlayer-id)
;;                   address-str #f)]))
;;      (define this-machine
;;        (ocapn-machine netlayer-id address-str hints-str))
     
;;      (values ocapn-url-type this-machine rest-paths)]
;;     [something-else
;;      (error 'not-an-ocapn-url "Not an ocapn url: ~a" something-else)]))

;; (define (url->ocapn-struct ocapn-url)
;;   (define-values (ocapn-url-type this-machine rest-paths)
;;     (ocapn-url-pieces ocapn-url))
;;   (match ocapn-url-type
;;     ;; This is a machine type, so we're done
;;     ["m" this-machine]
;;     ["s"
;;      (match-define (list encoded-swiss-num)
;;                    rest-paths)
;;      (ocapn-sturdyref this-machine (url-base64-decode encoded-swiss-num))]
;;     ["c"
;;      (match-define (list encoded-cert)
;;                    rest-paths)
;;      (define decoded-cert (syrup-decode (url-base64-decode encoded-cert)))
;;      (ocapn-cert this-machine decoded-cert)]
;;     ["u"
;;      (match-define (list encoded-cert key-type-and-private-key)
;;                    rest-paths)
;;      (define decoded-cert (syrup-decode (url-base64-decode encoded-cert)))
;;      (match-define (list key-type-str private-key-str)
;;                    (string-split key-type-and-private-key "."))
;;      (ocapn-bearer-union (ocapn-cert this-machine decoded-cert)
;;                          (string->symbol key-type-str)
;;                          (url-base64-decode private-key-str))])

;;   ;; way too lazy but the contracts do the right thing on these
;;   ;; TODO: Except kind of not because it says "broke its own contract",
;;   ;; a bit misleading
;;   (define-syntax-rule (define-url->ocapn-something id to-something)
;;     (define/contract (id ocapn-url)
;;       (-> url? to-something)
;;       (url->ocapn-struct ocapn-url))))
;; (define-url->ocapn-something url->ocapn-machine ocapn-machine?)
;; (define-url->ocapn-something url->ocapn-sturdyref ocapn-sturdyref?)
;; (define-url->ocapn-something url->ocapn-cert ocapn-cert?)
;; (define-url->ocapn-something url->ocapn-bearer-union ocapn-bearer-union?)

;; (define (string->ocapn-machine str)
;;   (url->ocapn-machine (string->url str)))
;; (define (string->ocapn-sturdyref str)
;;   (url->ocapn-sturdyref (string->url str)))
;; (define (string->ocapn-cert str)
;;   (url->ocapn-cert (string->url str)))
;; (define (string->ocapn-bearer-union str)
;;   (url->ocapn-bearer-union (string->url str)))
;; (define (string->ocapn-struct str)
;;   (url->ocapn-struct (string->url str)))

;; (define (ocapn-machine->string machine)
;;   (url->string (ocapn-machine->url machine)))
;; (define (ocapn-sturdyref->string sturdyref)
;;   (url->string (ocapn-sturdyref->url sturdyref)))
;; (define (ocapn-cert->string cert)
;;   (url->string (ocapn-cert->url cert)))
;; (define (ocapn-bearer-union->string bearer-union)
;;   (url->string (ocapn-bearer-union->url bearer-union)))

;; ;; helper procedure for the next two
;; (define (ocapn-machine->address-str machine)
;;   (match machine
;;     [(ocapn-machine transport address hints)
;;      (if hints
;;          (format "~a.~a.~a" transport address hints)
;;          (format "~a.~a" transport address))]))

;; (define (ocapn-machine->url machine)
;;   (define address-str (ocapn-machine->address-str machine))
;;   (url "ocapn" #f #f #f #f
;;        (list (path/param (string-append "m." address-str) '()))
;;        '() #f))

;; (define (ocapn-sturdyref->url sturdyref)
;;   (match sturdyref
;;     [(ocapn-sturdyref machine swiss-num)
;;      (define address-str (ocapn-machine->address-str machine))
;;      (url "ocapn" #f #f #f #f
;;           (list (path/param (string-append "s." address-str) '())
;;                 (path/param (url-base64-encode swiss-num) '()))
;;           '() #f)]))

;; (define (ocapn-cert->url cert)
;;   (error "TODO: implement ocapn-cert->url"))

;; (define (ocapn-bearer-union->url bearer-union)
;;   (error "TODO: implement ocapn-bearer-union->url"))

;; (define (ocapn-url? x)
;;   (and (url? x)
;;        (equal? (url-scheme x) "ocapn")))

;; (define-syntax-rule (define-ocapn-url-checker id prefix)
;;   (define (id x)
;;     (match x
;;       [(url "ocapn" #f #f #f #f (cons (path/param type-address '())
;;                                       _other-paths)
;;             '() #f)
;;        (string-prefix? type-address prefix)]
;;       [_ #f])))

;; (define-ocapn-url-checker ocapn-machine-url? "m.")
;; (define-ocapn-url-checker ocapn-sturdyref-url? "s.")
;; (define-ocapn-url-checker ocapn-cert-url? "c.")
;; (define-ocapn-url-checker ocapn-bearer-union-url? "u.")



;; (module+ test
;;   (require rackunit)

;;   (test-equal?
;;    "url->ocapn-machine, no hints"
;;    (url->ocapn-machine
;;     (string->url "ocapn:m.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"))
;;    (ocapn-machine 'onion
;;                   "wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
;;                   #f))

;;   (test-equal?
;;    "url->ocapn-machine, with hints"
;;    (string->ocapn-machine
;;     "ocapn:m.foo.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd.blablabla")
;;    (ocapn-machine 'foo
;;                   "wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
;;                   "blablabla"))

;;   (test-equal?
;;    "url->ocapn-sturdyref"
;;    (url->ocapn-sturdyref (string->url "ocapn:s.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd/eQjP4nR28ffX5eoNCK5DWH6DT_d7BIqD3-My459CUbU"))
;;    (ocapn-sturdyref
;;     (ocapn-machine 'onion
;;                    "wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
;;                    #f)
;;     #"y\b\317\342tv\361\367\327\345\352\r\b\256CX~\203O\367{\4\212\203\337\3432\343\237BQ\265"))

;;   (test-equal?
;;    "ocapn-machine->url, no hints"
;;    (ocapn-machine->url
;;     (ocapn-machine 'onion
;;                    "wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
;;                    #f))
;;    (string->url "ocapn:m.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"))

;;   (test-equal?
;;    "ocapn-machine->url, with hints"
;;    (ocapn-machine->url
;;     (ocapn-machine 'onion
;;                    "wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
;;                    "hintyhint"))
;;    (string->url "ocapn:m.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd.hintyhint"))

;;   (test-equal?
;;    "ocapn-sturdyref->url"
;;    (ocapn-sturdyref->url
;;     (ocapn-sturdyref
;;      (ocapn-machine 'onion
;;                     "wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
;;                     #f)
;;      #"y\b\317\342tv\361\367\327\345\352\r\b\256CX~\203O\367{\4\212\203\337\3432\343\237BQ\265"))
;;    (string->url "ocapn:s.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd/eQjP4nR28ffX5eoNCK5DWH6DT_d7BIqD3-My459CUbU"))

;;   (check-true
;;    (ocapn-machine-url? (string->url "ocapn:m.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd")))
;;   (check-true
;;    (ocapn-sturdyref-url? (string->url "ocapn:s.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd/eQjP4nR28ffX5eoNCK5DWH6DT_d7BIqD3-My459CUbU")))
;;   (check-true
;;    (ocapn-cert-url? (string->url "ocapn:c.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd/PDknc29tZS1jZXJ0NCd3aXRoNCdzb21lNCdkYXRhPg")))
;;   (check-true
;;    (ocapn-bearer-union-url? (string->url "ocapn:u.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd/PDknc29tZS1jZXJ0NCd3aXRoNCdzb21lNCdkYXRhPg/fake.IPu0dfs0j-uBRbNgF0gc6zTH2UGujJ-SLyAlhAABSW0")))
;;   (check-false
;;    (ocapn-sturdyref-url? (string->url "ocapn:m.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd")))

;;   (check-true
;;    (same-machine-location?
;;     (string->ocapn-machine "ocapn:m.testnet.foo")
;;     (string->ocapn-machine "ocapn:m.testnet.foo.blahblah")))
;;   (check-false
;;    (same-machine-location?
;;     (string->ocapn-machine "ocapn:m.testnet.foo")
;;     (string->ocapn-machine "ocapn:m.testnet.bar")))
;;   (check-false
;;    (same-machine-location?
;;     (string->ocapn-machine "ocapn:m.testnet.foo")
;;     (string->ocapn-machine "ocapn:m.othernet.foo")))
;;   (check-true
;;    (same-machine-location?
;;     (string->ocapn-machine "ocapn:m.testnet.foo")
;;     (string->ocapn-sturdyref "ocapn:s.testnet.foo/d1607782-3c39-463f-baae-408753681a91")))
;;   (check-false
;;    (same-machine-location?
;;     (string->ocapn-machine "ocapn:m.testnet.foo")
;;     (string->ocapn-sturdyref "ocapn:s.testnet.bar/d1607782-3c39-463f-baae-408753681a91"))))
;; )
