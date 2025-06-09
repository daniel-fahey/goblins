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
  #:use-module (goblins abstract-types)
  #:use-module (goblins utils ghash)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins contrib syrup)
  #:use-module (goblins utils crypto)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:export (make-syrup-store))

(define current-data-version 1)
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

;; In the change from portrait graph version 0 to 1 two new fields (aurie-vat-id
;; and roots-version) was added, to ensure old version 0 data is unmarshalled
;; correctly we need to provide a default value for those.
(define (current-version? version)
  (= version current-data-version))
(define portrait-graph-migrations
  (match-lambda
    ;; Version 0
    [(0 portraits slots)
     (let ((new-aurie-vat-id (strong-random-bytes 32))
           (roots-version 0))
       (make-portrait-graph current-data-version new-aurie-vat-id
                            roots-version portraits slots))]
    ;; There was a development branch with aurie-vat-id but no ugprade code,
    ;; lets add that just incase.
    ;; TODO: This development branch probably was only used by spritely, we
    ;; probably can remove this in a few versions.
    [(aurie-vat-id 0 portraits slots)
     (let ((roots-version 0))
       (make-portrait-graph current-data-version aurie-vat-id
                            roots-version portraits slots))]
    [((? current-version?) aurie-vat-id roots-version portraits slots)
     (make-portrait-graph current-data-version aurie-vat-id
                          roots-version portraits slots)]
    [something-else
     ;; TODO: Make aurie specific errors so they can be caught later.
     (error (format #f "Could not read portrait graph data, got: ~a"
                    something-else))]))

(define (portrait-graph-constructor . args)
  ;; In the future, use migrations macro and do the correct version check here.
  (portrait-graph-migrations args))

(define (portrait-graph-serialize label pg)
  (match pg
    [($ <portrait-graph> version aurie-vat-id roots-version portraits slots)
     (make-tagged* label version aurie-vat-id roots-version portraits slots)]))

(define-values (marshaller::portrait-graph unmarshaller::portrait-graph)
  (make-marshallers '<portrait-graph> portrait-graph? portrait-graph-constructor
                    portrait-graph-serialize))

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
  ;; If the process were to be terminated while writing to the backing
  ;; file, the save file would be corrupted.  To avoid this, we
  ;; instead write to a temporary file and then use the atomic rename
  ;; operation to replace the backing file.  Worst-case scenario is
  ;; that a junk temporary file will be left on the file system.
  (define port (mkstemp (string-append backing-file "-XXXXXX")))
  (define tmp-file (port-filename port))
  (syrup-write portrait-graph port #:marshallers marshallers)
  (close-port port)
  (rename-file tmp-file backing-file))

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
      (values aurie-vat-id roots-version saved-portraits saved-slots)]
     [(object-portrait slot)
      (unless (and saved-portraits saved-slots)
        (error "Cannot read an object from an empty store"))
      (hashq-ref saved-portraits slot)]))
  
  (make-persistence-store read-proc write-proc))
