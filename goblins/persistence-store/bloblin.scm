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

(define-module (goblins persistence-store bloblin)
  #:use-module (goblins core-types)
  #:use-module (goblins abstract-types)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins contrib syrup)
  #:use-module (goblins utils base32)
  #:use-module (goblins utils hashmap)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (make-bloblin-store))

;;; Welcome to Bloblin, a "reasonably disk efficient enough" Goblins
;;; storage system that relies on simple files.
;;;
;;; Notes on its design are below.
;;;
;;; /path/to/this-auriedb/<full-persistence-id>.bloblin
;;;
;;; - Then we write the header
;;;   #+BEGIN_SRC text
;;;     ['auriedb0 vat-aurie-id       ; aurie id of this vat
;;;                roots              ; these are assumed to not change from version to version
;;;                roots-version]     ; this is for upgrading roots
;;;   #+END_SRC
;;;
;;; - Then each churn. The first one is a "full" churn. Every one after
;;;   that is a delta.
;;;
;;;   #+BEGIN_SRC text
;;;     [write-time  ; seconds in UTC since unix epoch
;;;      new-types   ; described below
;;;      new-obj-ids ; described below
;;;      portraits]  ; a compressed (via accrued types and obj-ids) mapping
;;;   #+END_SRC
;;;
;;; - new-types is:
;;;
;;;   #+BEGIN_SRC text
;;;   {type-id-int: type-symbol}
;;;   #+END_SRC
;;;
;;; - new-obj-ids is:
;;;
;;;   #+BEGIN_SRC text
;;;   {obj-id-int: debug-name}
;;;   #+END_SRC
;;;
;;; To detect that a write was interrupted, we simply observe that the last
;;; expression fails to read from Syrup, then ignore that one.
;;;
;;; Many of these ideas could also be pretty easily moved to an indexeddb
;;; version.


;;; Path utilities
;;; ==============

(define (make-bloblin-file-path bloblin-dir root-churn-id)
  (string-join (list bloblin-dir
                     (string-append (number->string root-churn-id) ".bloblin"))
               file-name-separator-string))

;; To see why we need this, expose yourself to the following curse
;; by evaluating:
;;   (char-set->list char-set:digit)
(define char-set:integer-digit
  (string->char-set "0123456789"))

(define (file-name->root-churn-id filename)
  "Return integer associated with .bloblin filename, assuming it is one

If not, we return #f."
  (define churn-id-str
    (basename filename ".bloblin"))
  (and (string-every char-set:integer-digit churn-id-str)
       (string->number churn-id-str)))

(define (dir-contents->root-churn-ids dir-contents)
  "Get all the root churn ids as integers from DIR-CONTENTS list

Sorts and returns with largest first so that we can easily `car' off
the most recent version"
  (define root-churn-ids
    (fold
     (lambda (fname lst)
       (define churn-id
         (file-name->root-churn-id fname))
       (if churn-id
           (cons churn-id lst)
           lst))
     '() dir-contents))
  (sort root-churn-ids >))

(define (bloblin-vat-dir->root-churn-ids bloblin-vat-dir)
  "Take BLOBLIN-VAT-DIR and return a list of root churn ids inside"
  (if (file-exists? bloblin-vat-dir)
      (let* ((dir (opendir bloblin-vat-dir))
             (root-churn-ids
              (dir-contents->root-churn-ids
               (do ((entry (readdir dir) (readdir dir))
                    (lst '() (cons entry lst)))
                   ((eof-object? entry) lst)))))
        (closedir dir)
        root-churn-ids)
      '()))

(define (bloblin-vat-dir-next-churn-id bloblin-vat-dir)
  (match (bloblin-vat-dir->root-churn-ids bloblin-vat-dir)
    (() 0)
    ((latest-dir-num rest ...) (1+ latest-dir-num))))

;;; State for an active bloblin file
(define-record-type <bloblin-state>
  (make-bloblin-state file closed?
                      vat-aurie-id
                      types->type-ints type-ints->types
                      debug-names->debug-name-ints debug-name-ints->debug-names
                      next-type-id next-debug-name-id
                      roots roots-version
                      aurie-id->gen-id
                      generation->pos
                      next-gen-id)
  bloblin-state?
  ;; The open file recording state
  (file bloblin-state-file)
  ;; Whether or not this file is still open for reading/writing
  (closed? bloblin-state-closed? set-bloblin-state-closed?!)
  ;; The aurie id of this vat
  (vat-aurie-id bloblin-state-vat-aurie-id)
  ;; A mapping of types/debug names to their "compressed" identifiers
  (types->type-ints bloblin-state-types->type-ints)
  (type-ints->types bloblin-state-type-ints->types)
  (debug-names->debug-name-ints bloblin-state-debug-names->debug-name-ints)
  (debug-name-ints->debug-names bloblin-state-debug-name-ints->debug-names)
  ;; Counters for those compressed identifiers
  (next-type-id bloblin-state-next-type-id
                set-bloblin-state-next-type-id!)
  (next-debug-name-id bloblin-state-next-debug-name-id
                      set-bloblin-state-next-debug-name-id!)
  ;; The roots of this file
  (roots bloblin-state-roots)
  (roots-version bloblin-state-roots-version)
  ;; These store what generation in the file different entries are
  ;; stored at
  (aurie-id->gen-id bloblin-state-aurie-id->gen-id)
  ;;: TODO: We need some consistent naming around what's a "root churn"
  ;;; and what's a delta/generation
  ;; Map generations to positions in the file
  (generation->pos bloblin-state-generation->pos)
  ;; Despite all the Lore about the Data held in Aurie, this is
  ;; not a TNG reference. Store which generation is next
  ;; for the next write.
  (next-gen-id bloblin-state-next-gen-id
               set-bloblin-state-next-gen-id!))

(define (bloblin-state-close! bloblin-state)
  (close-port (bloblin-state-file bloblin-state))
  (set-bloblin-state-closed?! bloblin-state #t))

(define (bloblin-ensure-open bloblin-state)
  (when (bloblin-state-closed? bloblin-state)
    (error "Trying to operate on a closed bloblin file")))

;; This is intentionally a parameter, because when debugging
;; what's gone wrong with bloblin, you might not want to close
;; on an error, and instead debug the open file
(define %close-on-error? (make-parameter #t))

;; TODO: re-enable
(define (bloblin-close-on-error bloblin-state run-me)
  ;; (define (close-up err)
  ;;   (when (%close-on-error?)
  ;;     (bloblin-state-close! bloblin-state)))
  ;; (with-exception-handler close-up run-me)
  (run-me))

(define (increment-next-type-id! bloblin-state)
  (define cur-next-type-id
    (bloblin-state-next-type-id bloblin-state))
  (set-bloblin-state-next-type-id! bloblin-state (1+ cur-next-type-id)))

(define (increment-next-debug-name-id! bloblin-state)
  (define cur-next-debug-name-id
    (bloblin-state-next-debug-name-id bloblin-state))
  (set-bloblin-state-next-debug-name-id!
   bloblin-state (1+ cur-next-debug-name-id)))

(define (increment-next-gen-id! bloblin-state)
  (define cur-next-gen-id
    (bloblin-state-next-gen-id bloblin-state))
  (set-bloblin-state-next-gen-id!
   bloblin-state (1+ cur-next-gen-id)))

;; Update type ids / debug-name-ids if we've seen ids at or bigger
;; than the next type id
(define (maybe-increment-next-type-id! bloblin-state seen-type-id)
  (when (>= seen-type-id
            (bloblin-state-next-type-id bloblin-state))
    (set-bloblin-state-next-type-id! bloblin-state
                                     (1+ seen-type-id))))
(define (maybe-increment-next-debug-name-id! bloblin-state seen-debug-name-id)
  (when (>= seen-debug-name-id
            (bloblin-state-next-debug-name-id bloblin-state))
    (set-bloblin-state-next-debug-name-id! bloblin-state
                                           (1+ seen-debug-name-id))))

(define (setup-new-bloblin-file! roots roots-version
                                 bloblin-dir vat-aurie-id)
  (unless (file-exists? bloblin-dir)
    (mkdir bloblin-dir))
  ;; Find out what the next churn id is, open the relevant file
  (let* ((churn-ids (bloblin-vat-dir->root-churn-ids bloblin-dir))
         (latest-churn-id (match churn-ids
                            (() #f)
                            ((churn-id rest ...) churn-id)))
         (this-churn-id (if latest-churn-id
                            (1+ latest-churn-id)
                            0))
         (bloblin-file-path (make-bloblin-file-path
                             bloblin-dir this-churn-id)))
    ;; TODO: We should write to a temp file first then move it over


    ;; Maybe an unnecessary test; including for robustness for
    ;; now. Regardless if this existed, it *should have* appeared in
    ;; churn-ids.
    (when (file-exists? bloblin-file-path)
      (error "Bloblin file mysteriously appeared before opening, multiple writers possible"))
    (let ((bloblin-file
           (open-file bloblin-file-path "wb+")))
      ;; Write the header
      (syrup-write (make-tagged* 'bloblin0
                                 roots
                                 roots-version)
                   bloblin-file)
      (force-output bloblin-file)

      ;; Now return the new initialized bloblin-state
      (make-bloblin-state bloblin-file
                          #f
                          vat-aurie-id
                          (make-hash-table) (make-hash-table)
                          (make-hash-table) (make-hash-table)
                          0 0
                          roots roots-version
                          (make-hash-table)
                          (make-hash-table) 0))))

(define (write-generation! bloblin-state portraits)
  (define (%write-generation!)
    (match bloblin-state
      (($ <bloblin-state> bloblin-file _closed?
          vat-aurie-id
          types->type-ints type-ints->types
          debug-names->debug-name-ints debug-name-ints->debug-names
          next-type-id next-debug-name-id
          _roots _roots-version
          aurie-id->gen-id
          generation->pos next-gen-id)
       (define this-generation next-gen-id)

       ;; Used to record in aurie-id->gen-id where each of these
       ;; entries are
       (define cur-file-pos
         (ftell bloblin-file))

       ;; For tracking when we've added new types that we'll
       ;; record at the start of this generation entry
       (define new-types (make-hashvmap))
       (define new-debug-names (make-hashvmap))

       ;; Here we "compress" the portraits, but it's really just mapping
       ;; the aurie env types and debug names to integers.
       ;; Maybe further compression would happen in the future.
       (define compressed-portraits
         (hash-fold
          (lambda (k portrait hm)
            ;; Piece apart the portrait to compress it
            (match portrait
              ((type-name debug-name portrait-version portrait-data)
               (define type-id
                 (or (hash-ref types->type-ints type-name)
                     (let ((type-id (bloblin-state-next-type-id bloblin-state)))
                       (hash-set! types->type-ints type-name type-id)
                       (hashv-set! type-ints->types type-id type-name)
                       (increment-next-type-id! bloblin-state)
                       (set! new-types (hashmap-set new-types
                                                    type-id type-name))
                       type-id)))
               (define debug-name-id
                 (or (hash-ref debug-names->debug-name-ints debug-name)
                     (let ((debug-name-id (bloblin-state-next-debug-name-id
                                           bloblin-state)))
                       (hash-set! debug-names->debug-name-ints debug-name debug-name-id)
                       (hashv-set! debug-name-ints->debug-names debug-name-id debug-name)

                       (increment-next-debug-name-id! bloblin-state)
                       (set! new-debug-names (hashmap-set new-debug-names
                                                          debug-name-id debug-name))
                       debug-name-id)))
               (hashv-set! aurie-id->gen-id k this-generation)
               (hashmap-set hm k
                            (list type-id debug-name-id
                                  portrait-version portrait-data)))))
          (make-hashmap)
          portraits))

       (syrup-write (list (current-time)
                          new-types new-debug-names
                          compressed-portraits)
                    bloblin-file)
       (force-output bloblin-file)
       (hashv-set! generation->pos this-generation cur-file-pos)
       (increment-next-gen-id! bloblin-state))))

  (bloblin-ensure-open bloblin-state)
  (bloblin-close-on-error bloblin-state %write-generation!))

(define (open-bloblin-file-read-header bloblin-file-path)
  (define bloblin-file
    (open-file bloblin-file-path "rb+"))
  (define header
    (syrup-read bloblin-file))
  (when (eq? header the-eof-object)
    (error "Could not read header from bloblin file" bloblin-file-path))
  (unless (eq? (tagged-label header) 'bloblin0)
    (error "Wrong bloblin version, expecting bloblin0"))
  (match (tagged-data header)
    ((vat-aurie-id roots roots-version)
     (make-bloblin-state
      bloblin-file #f
      vat-aurie-id
      (make-hash-table) (make-hash-table)
      (make-hash-table) (make-hash-table)
      0 0
      roots roots-version
      (make-hash-table)
      (make-hash-table) 0))))

(define (bloblin-file-read-body bloblin-state)
  "Having already read the header of an open file within bloblin-state,
read the rest of the file and catch up BLOBLIN-STATE as appropriate."
  (define (%bloblin-file-read-only)
    (match bloblin-state
      (($ <bloblin-state> bloblin-file _closed?
          vat-aurie-id
          types->type-ints type-ints->types
          debug-names->debug-name-ints debug-name-ints->debug-names
          _next-type-id _next-debug-name-id
          _roots _roots-version
          aurie-id->gen-id
          generation->pos _next-gen-id)
       (let lp ()
         (define last-good-position (ftell bloblin-file))
         (define gen
           (syrup-read bloblin-file))
         (define this-generation-id
           (bloblin-state-next-gen-id bloblin-state))
         (match gen
           ((? eof-object?)
            ;; One way or another, we're done.
            ;; But first let's check if there was corruption from
            ;; an incomplete write of a generation.
            (unless (= (ftell bloblin-file) last-good-position)
              ;; Oops, indeed, it looks like this was corrupt!
              ;; Let's seek back and truncate the file.
              (seek bloblin-file last-good-position SEEK_SET)
              (truncate-file bloblin-file)
              (force-output bloblin-file)))
           ((_gen-time new-types new-debug-names portraits)
            ;; Update all the types
            (hashmap-for-each (lambda (type-id type-name)
                                (hash-set! types->type-ints
                                           type-name type-id)
                                (hashv-set! type-ints->types
                                            type-id type-name)
                                (maybe-increment-next-type-id! bloblin-state
                                                               type-id))
                              new-types)
            ;; Update the debug names
            (hashmap-for-each (lambda (debug-name-id debug-name)
                                (hash-set! debug-names->debug-name-ints
                                           debug-name debug-name-id)
                                (hashv-set! debug-name-ints->debug-names
                                            debug-name-id debug-name)
                                (maybe-increment-next-debug-name-id!
                                 bloblin-state debug-name-id))
                              new-debug-names)
            ;; Catch up the portraits
            (hashmap-for-each (lambda (aurie-id _portrait)
                                (hashv-set! aurie-id->gen-id aurie-id
                                            this-generation-id))
                              portraits)
            ;; Record which position this generation corresponds to
            ;; and increment that counter
            (hashv-set! generation->pos this-generation-id last-good-position)
            (increment-next-gen-id! bloblin-state)

            (lp)))))))
  (bloblin-ensure-open bloblin-state)
  (bloblin-close-on-error bloblin-state %bloblin-file-read-only))

(define (open-bloblin-file bloblin-file-path)
  "Open bloblin file and return <bloblin-state> object"
  (define bloblin-state
    (open-bloblin-file-read-header bloblin-file-path))
  (bloblin-file-read-body bloblin-state)
  bloblin-state)

;; When a user asks for the most recent generation, which is the
;; most common case, we can use this for an efficient way to read
;; only the relevant generations to slurp up all the relevant
;; portraits
(define (aurie-id-generation-mapping->visit-plan aurie-id->gen-id)
  (define visit-plan (make-hash-table))
  (hash-for-each
   (lambda (aurie-id gen-id)
     (hashv-set! visit-plan gen-id
                 (cons aurie-id
                       (hashv-ref visit-plan gen-id '()))))
   aurie-id->gen-id)
  (values visit-plan
          ;; A sorted itinerary
          (sort (hash-fold (lambda (key _val lst)
                             (cons key lst))
                           '()
                           visit-plan)
                <)))

(define (%insert-compressed-portrait! portraits
                                      aurie-id compressed-portrait
                                      debug-name-ints->debug-names
                                      type-ints->types)
  (match compressed-portrait
    ((type-id debug-name-id portrait-version portrait-data)
     (define debug-name
       (or (hashv-ref debug-name-ints->debug-names debug-name-id)
           (error "debug-name not found with this compressed id:" debug-name-id)))
     (define type
       (or (hashv-ref type-ints->types type-id)
           (error "type not found with this compressed id:" type-id)))
     (hashv-set! portraits aurie-id
                 (list type debug-name
                       portrait-version portrait-data)))))

;; An efficient way to get the most recent generation, which is the
;; most common case, based on indexed data in the bloblin-state. We
;; look at the mapping of all known aurie-ids to their most recent
;; geneartions. From there we look up the file positions of those
;; generations and we read only the relevant generations.
(define (bloblin-get-latest-generation bloblin-state)
  (define (%bloblin-get-latest-generation)
    (define bloblin-file (bloblin-state-file bloblin-state))
    (define old-pos (ftell bloblin-file))
    (define debug-name-ints->debug-names
      (bloblin-state-debug-name-ints->debug-names bloblin-state))
    (define type-ints->types
      (bloblin-state-type-ints->types bloblin-state))
    (define generation->pos
      (bloblin-state-generation->pos bloblin-state))
    (define-values (visit-plan itinerary)
      (aurie-id-generation-mapping->visit-plan
       (bloblin-state-aurie-id->gen-id bloblin-state)))
    (define portraits (make-hash-table))

    (do ((itinerary itinerary (cdr itinerary)))
        ((null? itinerary))
      (let* ((gen-id (car itinerary))
             (aurie-ids (hashv-ref visit-plan gen-id))
             (pos (hashv-ref generation->pos gen-id)))
        (seek bloblin-file pos SEEK_SET)
        (match (syrup-read bloblin-file)
          ((_write-time _nt _ndn compressed-portraits)
           (do ((aurie-ids aurie-ids (cdr aurie-ids)))
               ((null? aurie-ids))
             (match aurie-ids
               ((aurie-id . _rest-ids)
                (%insert-compressed-portrait!
                 portraits aurie-id
                 (hashmap-ref compressed-portraits aurie-id)
                 debug-name-ints->debug-names
                 type-ints->types))))))))
    (seek bloblin-file old-pos SEEK_SET)
    portraits)
  (bloblin-ensure-open bloblin-state)
  (bloblin-close-on-error bloblin-state %bloblin-get-latest-generation))

(define (bloblin-get-arbitrary-generation bloblin-state gen-id)
  (define (%bloblin-get-arbitrary-generation)
    (define bloblin-file (bloblin-state-file bloblin-state))
    (define old-pos (ftell bloblin-file))
    (define debug-name-ints->debug-names
      (bloblin-state-debug-name-ints->debug-names bloblin-state))
    (define type-ints->types
      (bloblin-state-type-ints->types bloblin-state))
    (define portraits (make-hash-table))
    (unless (>= gen-id 0)
      (error "gen-id must be an integer greater or equal to zero" gen-id))

    ;; Seek to start of file
    (seek bloblin-file 0 SEEK_SET)

    ;; Read the header, a no-op
    (let ((header (syrup-read bloblin-file)))
      (unless (and (tagged? header) (eq? (tagged-label header) 'bloblin0))
        (error "Expected bloblin0 header, got:" header)))

    ;; Now we read generations until we hit the gen-id specified
    (let lp ((i 0))
      (match (syrup-read bloblin-file)
        ((? eof-object?)
         (error "Requested generation-id exceeds entries in file"))
        ((_write-time _types _debug-names compressed-portraits)
         (hashmap-for-each
          (lambda (aurie-id compressed-portrait)
            (%insert-compressed-portrait!
             portraits aurie-id
             (hashmap-ref compressed-portraits aurie-id)
             debug-name-ints->debug-names
             type-ints->types))
          compressed-portraits)))
      ;; Loop unless this was the generation we were asked to stop at
      (unless (= i gen-id)
        (lp (1+ i))))

    (seek bloblin-file old-pos SEEK_SET)
    portraits)
  (bloblin-ensure-open bloblin-state)
  (bloblin-close-on-error bloblin-state %bloblin-get-arbitrary-generation))

(define (make-bloblin-store bloblin-dir)
  (define active-bloblin-state #f)  ; Active file to read/write from

  ;; We try to load bloblin state from a recent bloblin file, if that
  ;; exists. If it doesn't, we'll back out.
  (define (try-to-load-bloblin-state!)
    (unless active-bloblin-state
      ;; See if there's a "latest" bloblin state, load that
      (match (bloblin-vat-dir->root-churn-ids bloblin-dir)
        (() #f)
        ((latest-root-churn-id . rest-ids)
         (set! active-bloblin-state
               (open-bloblin-file
                (make-bloblin-file-path bloblin-dir latest-root-churn-id)))))))

  (define (get-active-or-chosen-bloblin-state root-churn-id)
    (if root-churn-id
        (open-bloblin-file (make-bloblin-file-path bloblin-dir root-churn-id))
        ;; no root-churn-id provided, so return the active-bloblin-state
        (begin
          (try-to-load-bloblin-state!)
          active-bloblin-state)))

  (define memory-read-proc
    (methods
     [(graph-and-slots #:key root-churn-id delta-id)
      (define bloblin-state
        (get-active-or-chosen-bloblin-state root-churn-id))
      (if bloblin-state
          (let ((portraits
                 (if delta-id
                     (bloblin-get-arbitrary-generation bloblin-state delta-id)
                     (bloblin-get-latest-generation bloblin-state))))
            (when root-churn-id
              (bloblin-state-close! bloblin-state))
            (values (bloblin-state-vat-aurie-id bloblin-state)
                    (bloblin-state-roots-version bloblin-state)
                    portraits
                    (bloblin-state-roots bloblin-state)))
          (values #f #f #f #f))]
     ;; TODO: We can do a way more efficient version of this, just
     ;; trying to get this out the door
     [(object-portrait slot #:key root-churn-id delta-id)
      (define bloblin-state
        (get-active-or-chosen-bloblin-state root-churn-id))
      (unless bloblin-state
        (error "Cannot read object from empty store" slot))
      (define portraits
        (if delta-id
            (bloblin-get-arbitrary-generation bloblin-state delta-id)
            (bloblin-get-latest-generation bloblin-state)))
      (when root-churn-id
        (bloblin-state-close! bloblin-state))
      (hash-ref portraits slot)]
     ;; @@: Could be more efficient, fine for now.
     ;; This is meant to be a tool for the REPL in the future, or something.
     ;; Playing around in the meanwhile.
     #;[(get-generations)
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
             generations))]
     ))

  (define memory-save-proc
    (methods
     [(save-graph vat-aurie-id version portraits roots)
      (when (and active-bloblin-state
                 (not (bloblin-state-closed? active-bloblin-state)))
        (bloblin-state-close! active-bloblin-state))
      ;; Reset the state and setup a new bloblin file for this generation
      (set! active-bloblin-state
            (setup-new-bloblin-file! roots version bloblin-dir vat-aurie-id))
      ;; Now write out this particular generation
      (write-generation! active-bloblin-state portraits)]
     [(save-delta delta-portraits)
      ;; We can skip writing deltas if there's nothing changed.
      ;; @@: Should we do the bail-out on saving a delta in vats
      ;; themselves? It definitely makes sense to skip them here.
      (unless (zero? (hash-count (const #t) delta-portraits))
        (try-to-load-bloblin-state!)
        (unless active-bloblin-state
          (error "Tried to save bloblin delta without having saved full graph"))
        (write-generation! active-bloblin-state delta-portraits))]))

  (make-persistence-store memory-read-proc memory-save-proc))
