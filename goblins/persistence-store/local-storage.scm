;;; Copyright 2025 Jessica Tallon
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

(define-library (goblins persistence-store local-storage)
  (export make-local-storage-store)
  (cond-expand
   (guile
    (import (scheme base)))
   (hoot
    (import (guile)
            (goblins core-types)
            (goblins abstract-types)
            (goblins actor-lib methods)
            (goblins contrib syrup)
            (goblins contrib base64)
            (goblins utils hashmap)
            (goblins utils crypto)
            (hoot ffi)
            (ice-9 match)
            (srfi srfi-9))))

  (begin
    (cond-expand
     (guile
      (define (make-local-storage-store name)
        (error "local storage not available on Guile VM")))
     (hoot
      ;; Hoot FFI
      (define-foreign local-storage-ref
        "localStorage" "getItem"
        (ref string) -> (ref null string))
      (define-foreign local-storage-set!
        "localStorage" "setItem"
        (ref string) (ref string) -> none)

      (define data-version 0)
      (define-record-type <portrait-graph>
        (make-portrait-graph version aurie-vat-id roots-version portraits slots)
        portrait-graph?
        (aurie-vat-id portrait-graph-aurie-vat-id)
        ;; Version for the <portrait-graph>
        (version portrait-graph-version)
        ;; Version used by the vat for the roots
        (roots-version portrait-graph-roots-version)
        (portraits portrait-graph-portraits)
        (slots portrait-graph-slots))

      (define (portrait-graph-serialize label pg)
        (match pg
          [($ <portrait-graph> version aurie-vat-id roots-version portraits slots)
           (make-tagged* label version aurie-vat-id roots-version portraits slots)]))

      (define-values (marshaller::portrait-graph unmarshaller::portrait-graph)
        (make-marshallers '<portrait-graph> portrait-graph? make-portrait-graph
                          portrait-graph-serialize))

      (define marshallers (list marshaller::portrait-graph))
      (define unmarshallers (list unmarshaller::portrait-graph))

      (define (ghash->hash-table gh)
        (define ht (make-hash-table))
        (hashmap-for-each
         (lambda (key value)
           (hashq-set! ht key value))
         gh)
        ht)

      (define (read-depictions name)
        (match (local-storage-ref name)
          (#f (values #f #f #f #f))
          (b64-data
           (let* ((data (base64-decode b64-data))
                  (graph (syrup-decode data #:unmarshallers unmarshallers)))
             ;; hash tables in syrup aren't round trippable, all of them will come
             ;; back as ghashes, we need to convert as we expect a hash table
             ;; FIXME: see issue https://codeberg.org/spritely/goblins/issues/753
             (define portraits-as-ghash (portrait-graph-portraits graph))
             (define portraits-as-ht (ghash->hash-table portraits-as-ghash))
             (values (portrait-graph-aurie-vat-id graph)
                     (portrait-graph-roots-version graph)
                     portraits-as-ht
                     (portrait-graph-slots graph))))))

      (define (write-depictions name aurie-vat-id version portraits slots)
        (define portrait-graph
          (make-portrait-graph aurie-vat-id data-version version portraits slots))
        (define data
          (syrup-encode portrait-graph #:marshallers marshallers))
        ;; The data is a bv, normally we'd convert it to a uint8array but
        ;; actually local storage is string only so base64 it is...
        (define b64-data (base64-encode data))
        (local-storage-set! name b64-data))

      (define (make-local-storage-store name)
        (define-values (aurie-vat-id roots-version saved-portraits saved-slots)
          (read-depictions name))

        (define write-proc
          (methods
           [(save-graph vat-id version portraits slots)
            (set! aurie-vat-id vat-id)
            (set! saved-portraits portraits)
            (set! saved-slots slots)
            (set! roots-version version)
            (write-depictions name aurie-vat-id version portraits slots)]
           [(save-delta portraits)
            (unless (and saved-portraits saved-slots) (delta-before-graph-error))
            (hash-for-each
             (lambda (slot new-portrait-data)
               (hashq-set! saved-portraits slot new-portrait-data))
             portraits)
            (write-depictions name aurie-vat-id roots-version saved-portraits
                              saved-slots)]))

        (define read-proc
          (methods
           [(graph-and-slots)
            (values aurie-vat-id roots-version saved-portraits saved-slots)]
           [(object-portrait slot)
            (unless (and saved-portraits saved-slots) (empty-store-error))
            (hashq-ref saved-portraits slot)]))

        (make-persistence-store read-proc write-proc))))))
