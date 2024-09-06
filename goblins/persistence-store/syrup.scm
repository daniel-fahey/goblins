;;; Copyright 2024 Jessica Tallon
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

(define-module (goblins persistence-store syrup)
  #:use-module (goblins core-types)
  #:use-module (goblins ghash)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins contrib syrup)
  ;; Slightly strange these are in ocapn as they aren't specific to them.
  #:use-module (goblins ocapn marshalling)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (gcrypt random)
  #:export (make-syrup-store))

(define current-data-version 1)
(define-record-type <portrait-graph>
  (make-portrait-graph aurie-vat-id version roots-version portraits slots)
  portrait-graph?
  (aurie-vat-id portrait-graph-aurie-vat-id)
  ;; Version for the <portrait-graph>
  (version portrait-graph-version)
  ;; Version used by the vat for the roots
  (roots-version portrait-graph-roots-version)
  (portraits portrait-graph-portraits)
  (slots portrait-graph-slots))

;; In the change from portrait graph version 0 to 1 two new fields (aurie-vat-id
;; and roots-version) was added, to ensure old version 0 data is unmarshalled
;; correctly we need to provide a default value for those.
(define portrait-graph-unmarshall-constructor
  (match-lambda
    ;; Version 0
    [(0 portraits slots)
     (let ((new-aurie-vat-id (gen-random-bv 32 %gcry-strong-random))
           (roots-version 0))
       (make-portrait-graph new-aurie-vat-id current-data-version
                            roots-version portraits slots))]
    ;; There was a development branch with aurie-vat-id but no ugprade code,
    ;; lets add that just incase.
    ;; TODO: This development branch probably was only used by spritely, we
    ;; probably can remove this in a few versions.
    [(aurie-vat-id 0 portraits slots)
     (let ((roots-version 0))
       (make-portrait-graph aurie-vat-id current-data-version
                            roots-version portraits slots))]
    [(aurie-vat-id graph-version roots-version portraits slots)
     (if (equal? graph-version current-data-version)
         (make-portrait-graph aurie-vat-id graph-version roots-version portraits
                              slots)
         (error "Unknown portrait graph version"))]))

(define-values (marshaller::portrait-graph unmarshaller::portrait-graph)
  (make-marshallers <portrait-graph>))

(define marshallers
  (list marshaller::portrait-graph))
(define unmarshallers
  (list unmarshaller::portrait-graph))

(define (read-depictions backing-file)
  (if (file-exists? backing-file)
      (call-with-input-file backing-file
        (lambda (port)
          (define portrait-graph
            (syrup-read port #:unmarshallers unmarshallers))
          (unless (eq? (portrait-graph-version portrait-graph) current-data-version)
            (error "Portrait data is different version than supported"
                   (portrait-graph-version portrait-graph)))

          ;; Syrup writes both hash-table and ghash as syrup hashmaps
          ;; this is fine, but it has no way to know we want a hash-table
          ;; not a ghash in return and it picks ghash, lets convert.
          (define portraits-as-ghash
            (portrait-graph-portraits portrait-graph))

          (define portraits-as-hash-table
            (make-hash-table))
          (ghash-for-each
           (lambda (key value)
             (hashq-set! portraits-as-hash-table key value))
           portraits-as-ghash)
            
          (values (portrait-graph-aurie-vat-id portrait-graph)
                  (portrait-graph-roots-version portrait-graph)
                  portraits-as-hash-table
                  (portrait-graph-slots portrait-graph))))
      (values #f #f #f #f)))
(define (write-depictions backing-file aurie-vat-id version portraits slots)
  (define portrait-graph
    (make-portrait-graph aurie-vat-id current-data-version version portraits slots))
  (call-with-output-file backing-file
    (lambda (port)
      (syrup-write portrait-graph port #:marshallers marshallers))))

(define* (make-syrup-store backing-file)
  (define-values (aurie-vat-id roots-version saved-portraits saved-slots)
    (read-depictions backing-file))

  (define write-proc
    (methods
     [(save-graph vat-id version portraits slots)
      (set! aurie-vat-id vat-id)
      (set! saved-portraits portraits)
      (set! saved-slots slots)
      (set! roots-version version)
      (write-depictions backing-file aurie-vat-id version portraits slots)]
     [(save-delta portraits)
      (unless (and saved-portraits saved-slots)
        (error "Cannot save deltas until a whole graph has been stored first"))
      (hash-for-each
       (lambda (slot new-portrait-data)
         (hashq-set! saved-portraits slot new-portrait-data))
       portraits)
      (write-depictions backing-file aurie-vat-id roots-version saved-portraits
                        saved-slots)]))

  (define read-proc
    (methods
     [(graph-and-slots)
      (values aurie-vat-id saved-portraits saved-slots)]
     [(object-portrait slot)
      (unless (and saved-portraits saved-slots)
        (error "Cannot read an object from an empty store"))
      (hashq-ref saved-portraits slot)]))
  
  (make-persistence-store read-proc write-proc))
