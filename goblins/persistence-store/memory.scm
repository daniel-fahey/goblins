;;; Copyright 2024 Jessica Tallon
;;; Copyright 2025 Christine Lemmer-Webber
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

(define-module (goblins persistence-store memory)
  #:use-module (goblins core-types)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins utils hashmap)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:export (make-memory-store))

(define-record-type <full-churn-data>
  (make-full-churn-data roots-version roots)
  full-churn-data?
  (roots-version full-churn-data-roots-version)
  (roots full-churn-data-roots))

(define-record-type <churn-entry>
  (make-churn-entry churn-id portraits full-churn-data)
  churn-entry?
  (churn-id churn-entry-churn-id)
  (portraits churn-entry-portraits)
  (full-churn-data churn-entry-full-churn-data))  ; if #f, this is a delta

(define (churn-entry-delta? churn-entry)
  (not (churn-entry-full-churn-data churn-entry)))

(define (make-memory-store)
  "Provides a simple in memory store"

  (define vat-aurie-id #f)  ; set every time we save-graph
  (define portrait-revisions (make-hashvmap))
  (define last-churn-id #f)
  (define next-churn-id 0)

  ;; Utility procedure. Gathers up from `revision' all the way up to
  ;; whatever the latest full churn version is
  (define (gather-defns revision)
    (define portraits (make-hash-table))
    (define (traverse-revisions revision)
      (define churn-entry
        (hashmap-ref portrait-revisions revision))
      (define (write-portraits!)
        (hash-for-each
         (lambda (key value)
           (hashq-set! portraits key value))
         (churn-entry-portraits churn-entry)))
      (cond
       ;; We've found a full revision! We can
       ;; stop here and write out
       ((churn-entry-full-churn-data churn-entry)
        =>
        (lambda (full-churn-data)
          ;; Write out the root
          (write-portraits!)
          ;; Return this information, which will be pushed back
          ;; up the chain
          (values (full-churn-data-roots full-churn-data)
                  (full-churn-data-roots-version full-churn-data))))
       ;; Otherwise, add the delta
       (else
        (let-values ([(roots roots-version)
                      ;; We need to recurse so that we write out all
                      ;; the older versions *first*, and then we
                      ;; clobber them with the newer versions
                      (traverse-revisions (1- revision))])
          (write-portraits!)
          (values roots roots-version)))))

    (define-values (roots roots-version)
      (traverse-revisions revision))
    (values portraits roots roots-version))

  (define (error-if-no-portraits)
    (unless last-churn-id
      (persistence-error "No portrait data has been stored")))

  (define memory-read-proc
    (methods
     [(graph-and-slots #:key (churn-id last-churn-id))
      (if last-churn-id
          (let-values ([(stored-portraits stored-roots roots-version)
                        (gather-defns churn-id)])
            (values vat-aurie-id roots-version stored-portraits stored-roots))
          (values #f #f #f #f))]
     [(object-portrait slot #:key (churn-id last-churn-id))
      (error-if-no-portraits)
      (let-values ([(stored-portraits _roots _roots-version)
                    (gather-defns churn-id)])
        (hashq-ref stored-portraits slot))]
     [(next-churn-id) next-churn-id]
     ;; @@: Could be more efficient, fine for now.
     ;; This is meant to be a tool for the REPL in the future, or something.
     ;; Playing around in the meanwhile.
     [(get-generations)
      (error-if-no-portraits)
      ;; We actually pull the generations off of here and sort them
      ;; just in case we allow dropping generations in the future of
      ;; this API
      (let ((generations (sort (hashmap-fold
                                (lambda (k _v result)
                                  (cons k result))
                                '()
                                portrait-revisions)
                               <)))
        (map (lambda (i)
               (let* ((entry (hashmap-ref portrait-revisions i))
                      (portraits (churn-entry-portraits entry))
                      (num-refrs (hash-fold (lambda (_k _v result)
                                              (1+ result))
                                            0 portraits)))
                 ;; We may eventually put other info in here
                 `(,i (delta? ,(churn-entry-delta? entry))
                      (num-portraits ,num-refrs)
                      ,@(cond
                         [(churn-entry-full-churn-data entry)
                          =>
                          (lambda (fc-data)
                            `((roots ,(full-churn-data-roots fc-data))))]
                         [else '()]))))
             generations))]))
      
  (define (save-churn! portraits full-churn-data)
    (let ([churn-entry (make-churn-entry next-churn-id
                                         portraits
                                         full-churn-data)])
      (set! portrait-revisions (hashmap-set portrait-revisions next-churn-id
                                            churn-entry))
      ;; increment the ids
      (set! last-churn-id next-churn-id)
      (set! next-churn-id (1+ next-churn-id))))

  (define memory-save-proc
    (methods
     [(save-graph aurie-id version portraits roots)
      (set! vat-aurie-id aurie-id)
      (save-churn! portraits
                   (make-full-churn-data version roots))]
     [(save-delta delta-portraits)
      (error-if-no-portraits)
      (save-churn! delta-portraits #f)]))
  (make-persistence-store memory-read-proc memory-save-proc))

