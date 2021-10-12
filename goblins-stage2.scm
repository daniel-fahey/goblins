;;; Copyright 2019-2021 Christine Lemmer-Webber
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

;; STAGE 2: Add:
;;  - transactormaps
;;  - actormap-poke!
;;  - actormap-peek

(define-module (goblins stage2)
  #:export (make-whactormap
            make-actormap

            spawn S

            actormap-spawn
            actormap-spawn!
            ;; actormap-spawn-mactor!

            actormap-turn*
            actormap-turn

            actormap-peek
            actormap-poke!
            actormap-reckless-poke!

            actormap-run
            actormap-run!
            actormap-run*

            whactormap?
            transactormap?
            transactormap-merge!

            ;;;; yet to come:
            ;; <- <-np on
            )
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match))

;; Old hack to get the "unspecified/undefined type"
(define _void (if #f #f))

(define-record-type <actormap>
  ;; TODO: This is confusing, naming-wise? (see make-actormap alias)
  (_make-actormap metatype data vat-connector)
  actormap?
  (metatype actormap-metatype)
  (data actormap-data)
  (vat-connector actormap-vat-connector))

(set-record-type-printer!
 <actormap>
 (lambda (am port)
   (format port "#<actormap ~a>" (actormap-metatype-name (actormap-metatype am)))))

(define-record-type <actormap-metatype>
  (make-actormap-metatype name ref-proc set!-proc)
  actormap-metatype?
  (name actormap-metatype-name)
  (ref-proc actormap-metatype-ref-proc)
  (set!-proc actormap-metatype-set!-proc))

(define (actormap-set! am key val)
  ((actormap-metatype-set!-proc (actormap-metatype am))
   am key val)
  _void)

;; (-> actormap? local-refr? (or/c mactor? #f))
(define (actormap-ref am key)
  ((actormap-metatype-ref-proc (actormap-metatype am)) am key))

;; Weak-hash actormaps
;; ===================

(define-record-type <whactormap-data>
  (make-whactormap-data wht)
  whactormap-data?
  (wht whactormap-data-wht))

(define (whactormap-ref am key)
  (define wht (whactormap-data-wht (actormap-data am)))
  (hashq-ref wht key #f))

(define (whactormap-set! am key val)
  (define wht (whactormap-data-wht (actormap-data am)))
  (hashq-set! wht key val))

(define whactormap-metatype
  (make-actormap-metatype 'whactormap whactormap-ref whactormap-set!))

(define* (make-whactormap #:key [vat-connector #f])
  (_make-actormap whactormap-metatype
                  (make-whactormap-data (make-weak-key-hash-table))
                  vat-connector))

(define (whactormap? obj)
  (and (actormap? obj)
       (eq? (actormap-metatype obj) whactormap-metatype)))

;; TODO: again, confusing (see <actormap>)
(define make-actormap make-whactormap)


;; Transactional actormaps
;; =======================

(define-record-type <transactormap-data>
  (make-transactormap-data parent delta merged?)
  transactormap-data?
  (parent transactormap-data-parent)
  (delta transactormap-data-delta)
  (merged? transactormap-data-merged? set-transactormap-data-merged?!))

(define (transactormap-merged? transactormap)
  (transactormap-data-merged? (actormap-data transactormap)))

(define (transactormap-ref transactormap key)
  (define tm-data (actormap-data transactormap))
  (when (transactormap-data-merged? tm-data)
    (error "Can't use transactormap-ref on merged transactormap"))
  (define tm-delta
    (transactormap-data-delta tm-data))
  (define tm-val (hashq-ref tm-delta key #f))
  (if tm-val
      ;; we got it, it's in our delta
      tm-val
      ;; search parents for key
      (let ([parent (transactormap-data-parent tm-data)])
        (actormap-ref parent key))))

(define (transactormap-set! transactormap key val)
  (when (transactormap-merged? transactormap)
    (error "Can't use transactormap-set! on merged transactormap"))
  (define tm-delta (transactormap-data-delta (actormap-data transactormap)))
  (hashq-set! tm-delta key val)
  _void)

;; Not threadsafe, but probably doesn't matter
(define (transactormap-merge! transactormap)
  ;; Serves two functions:
  ;;  - to extract the root weak-hasheq
  ;;  - to merge this transaction on top of the weak-hasheq
  (define (do-merge! transactormap)
    (define tm-data (actormap-data transactormap))
    (define parent (transactormap-data-parent tm-data))
    (define parent-mtype (actormap-metatype parent))
    ;; TODO: Should we actually return the root-wht instead,
    ;;   since that's what we're comitting to?
    (define root-actormap
      (cond
       [(eq? parent-mtype whactormap-metatype)
        parent]
       [(eq? parent-mtype transactormap)
        (do-merge! parent)]
       [else
        (error (format #f "Actormap metatype not supported for merging: ~a"
                       parent-mtype))]))
    ;; Optimization: we pull out the root weak hash table here and
    ;; merge it
    (define root-wht (whactormap-data-wht (actormap-data root-actormap)))
    (unless (transactormap-data-merged? tm-data)
      (hash-for-each
       (lambda (key val)
         (hashq-set! root-wht key val))
       (transactormap-data-delta tm-data))
      (set-transactormap-data-merged?! tm-data #t))
    root-actormap)
  (do-merge! transactormap)
  _void)

(define transactormap-metatype
  (make-actormap-metatype 'transactormap transactormap-ref transactormap-set!))

(define (make-transactormap parent)
  (define vat-connector (actormap-vat-connector parent))
  (_make-actormap transactormap-metatype
                  (make-transactormap-data parent (make-hash-table) #f)
                  vat-connector))


;; Ref(r)s
;; =======

(define-record-type <local-object-refr>
  (make-local-object-refr debug-name vat-connector)
  local-object-refr?
  (debug-name local-object-refr-debug-name)
  (vat-connector local-object-refr-vat-connector))

(set-record-type-printer!
 <local-object-refr>
 (lambda (lor port)
   (match (local-object-refr-debug-name lor)
     [#f (display "#<local-object>" port)]
     [debug-name
      (format port "#<local-object ~a>" debug-name)])))

(define-record-type <local-promise-refr>
  (make-local-promise-refr vat-connector)
  local-promise-refr?
  (vat-connector local-promise-refr-vat-connector))

(set-record-type-printer!
 <local-promise-refr>
 (lambda (lpr port)
   (display "#<local-promise>" port)))

(define (local-refr? obj)
  (or (local-object-refr? obj) (local-promise-refr? obj)))

(define (local-refr-vat-connector local-refr)
  (match local-refr
    [(? local-object-refr?)
     (local-object-refr-vat-connector local-refr)]
    [(? local-promise-refr?)
     (local-promise-refr-vat-connector local-refr)]))


(define (live-refr? obj)
  (or (local-refr? obj)
      ;; TODO: Finish as we fill in the other refr types
      ))

;; "Become" sealer/unsealers
;; =========================

(define (make-become-sealer-triplet)
  (define-record-type <become-seal>
    (make-become-seal new-behavior return-val)
    become-sealed?
    (new-behavior unseal-behavior)
    (return-val unseal-return-val))
  (define* (become new-behavior #:optional [return-val _void])
    (make-become-seal new-behavior return-val))
  (define (unseal sealed)
    (values (unseal-behavior sealed)
            (unseal-return-val sealed)))
  (values become unseal become-sealed?))

;; Mactors
;; =======

;; Starting with the simplest.

(define-record-type <mactor:object>
  (mactor:object behavior become-unsealer become?)
  mactor:object?
  (behavior mactor:object-behavior)
  (become-unsealer mactor:object-become-unsealer)
  (become? mactor:object-become?))

;; Re-entry Protection
;; ===================
;;
;; Or rather, re-entry protection goes here.
;; This works fairly ideally in Racket; in Guile the
;; with-continuation-barrier version below... well it's complicated because
;; unlike in Racket, it's not "marking the stack" to prevent re-entry but
;; permitting exceptions to "move upward" as it were... so we have to do
;; that ourselves, and it's kind of a mess.  No idea what the performance
;; implications are.

(define %re-entry-protect (make-parameter #f))

(define (_re-protec proc)
  (define result
    ;; protect against re-entrancy attacks
    ;; (... but also "protects" against live debugging, unfortunately)
    (with-continuation-barrier
     (lambda ()
       (with-exception-handler
           (lambda (exn)
             (list 'error exn))
         (lambda ()
           ;; actors are only permitted one value from their
           ;; continuation
           (define result
             (proc))
           (list 'success result))
         #:unwind? #t
         #:unwind-for-type #t))))
  (match result
    [('success result)
     result]
    [('error err)
     (raise-exception err)]))

(define (with-re-entry-protection proc)
  (if (%re-entry-protect)
      (_re-protec proc)
      (proc)))


;; Syscaller
;; =========

;; Do NOT export this esp under serious ocap confinement
(define current-syscaller (make-parameter #f))

(define (fresh-syscaller actormap)
  (define vat-connector
    (actormap-vat-connector actormap))
  (define new-msgs '())

  (define closed? #f)

  (define (this-syscaller method-id . args)
    (when closed?
      (error "Sorry, this syscaller is closed for business!"))
    (define method
      (case method-id
        [(S) _S]
        [(spawn) _spawn]
        ['spawn-mactor spawn-mactor]
        [(vat-connector) get-vat-connector]
        [(near-refr?) near-refr?]
        [(near-mactor) near-mactor]
        [else (error 'invalid-syscaller-method
                     "~a" method-id)]))
    (apply method args))

  ;; TODO
  (define (near-refr? obj)
    (and (local-refr? obj)
         (eq? (local-refr-vat-connector obj)
              vat-connector)))

  (define (near-mactor refr)
    (actormap-ref actormap refr))

  (define (get-vat-connector)
    vat-connector)

  (define (actormap-ref-or-die to-refr)
    (define mactor
      (actormap-ref actormap to-refr))
    (unless mactor
      (error 'no-such-actor "no actor with this id in this vat: ~a" to-refr))
    mactor)

  ;; call actor's behavior
  (define (_S to-refr args)
    ;; Restrict to live-refrs which appear to have the same
    ;; vat-connector as us
    (unless (local-refr? to-refr)
      (error 'not-callable
             "Not a live reference: ~a" to-refr))

    (unless (eq? (local-refr-vat-connector to-refr)
                 vat-connector)
      (error 'not-callable
             "Not in the same vat: ~a" to-refr))

    (define mactor
      (actormap-ref-or-die to-refr))

    (match mactor
      [(? mactor:object?)
       (let ((actor-behavior
              (mactor:object-behavior mactor))
             (become?
              (mactor:object-become? mactor))
             (become-unsealer
              (mactor:object-become-unsealer mactor)))
         ;; I guess watching for this guarantees that an immediate call
         ;; against a local actor will not be tail recursive.
         ;; TODO: We need to document that.
         (define-values (new-behavior return-val)
           (let ([returned
                  (with-re-entry-protection
                   (lambda ()
                     (apply actor-behavior args)))])
             (if (become? returned)
                 ;; The unsealer unseals both the behavior and return-value anyway
                 (become-unsealer returned)
                 ;; In this case, we're not becoming anything, so just give us
                 ;; the return-val
                 (values #f returned))))

         ;; if a new behavior for this actor was specified,
         ;; let's replace it
         (when new-behavior
           (unless (procedure? new-behavior)
             (error 'become-failure "Tried to become a non-procedure behavior: ~s"
                    new-behavior))
           (actormap-set! actormap to-refr
                          (mactor:object
                           new-behavior
                           (mactor:object-become-unsealer mactor)
                           (mactor:object-become? mactor))))

         return-val)]
      ;; ;; If it's an encased value, "calling" it just returns the
      ;; ;; internal value.
      ;; [(? mactor:encased?)
      ;;  (mactor:encased-val mactor)]
      ;; ;; Ah... we're linking to another actor locally, so let's
      ;; ;; just de-symlink and call that instead.
      ;; [(? mactor:local-link?)
      ;;  (keyword-apply _S kws kw-vals
      ;;                 (mactor:local-link-point-to mactor)
      ;;                 args)]
      ;; Not a callable mactor!
      [_other
       (error 'not-callable
              "Not an encased or object mactor: ~a" mactor)]))

  ;; spawn a new actor
  (define (_spawn constructor args debug-name)
    (define-values (become become-unsealer become-sealed?)
      (make-become-sealer-triplet))
    (define initial-behavior
      (apply constructor become args))
    (match initial-behavior
      ;; New procedure, so let's set it
      [(? procedure?)
       (let ((actor-refr
              (make-local-object-refr debug-name vat-connector)))
         (actormap-set! actormap actor-refr
                        (mactor:object initial-behavior
                                       become-unsealer become-sealed?))
         actor-refr)]
      ;; If someone returns another actor, just let that be the actor
      [(? live-refr? pre-existing-refr)
       pre-existing-refr]
      [_
       (error 'invalid-actor-handler "Not a procedure or live refr: ~a" initial-behavior)]))

  (define (spawn-mactor mactor debug-name)
    (actormap-spawn-mactor! actormap mactor debug-name))

  (define (get-internals)
    (list actormap new-msgs))

  (define (close-up!)
    (set! closed? #t))

  (values this-syscaller get-internals close-up!))

(define (call-with-fresh-syscaller am proc)
  (define-values (sys get-sys-internals close-up!)
    (fresh-syscaller am))
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (parameterize ([current-syscaller sys])
        (proc sys get-sys-internals)))
    (lambda ()
      (close-up!))))

;; Internal utilities
;; ==================

(define (get-syscaller-or-die)
  (define sys (current-syscaller))
  (unless sys
    (error "No current syscaller"))
  sys)

;; Core API
;; ========

;; System calls
(define (spawn constructor . args)
  (define sys (get-syscaller-or-die))
  (sys 'spawn constructor args (procedure-name constructor)))
(define (S refr . args)
  (define sys (get-syscaller-or-die))
  (sys 'S refr args))
(define (<- refr . args)
  (define sys (get-syscaller-or-die))
  (sys '<- refr args))
(define (<-np refr . args)
  (define sys (get-syscaller-or-die))
  (sys '<-np refr args))

(define* (on vow #:optional (fulfilled-handler #f)
             #:key
             [catch #f]
             [finally #f]
             [promise? #f])
  'TODO)

(define (spawn-promise-values)
  'TODO)
(define (spawn-promise-cons)
  'TODO)


;; Spawning
;; ========

;; This is the internally used version of actormap-spawn,
;; also used by the syscaller.  It doesn't set up a syscaller
;; if there isn't currently one.
(define* (actormap-spawn!* actormap actor-constructor
                           args
                           #:optional
                           [debug-name (procedure-name actor-constructor)])
  (define vat-connector
    (actormap-vat-connector actormap))
  (define-values (become become-unseal become?)
    (make-become-sealer-triplet))
  (define actor-handler
    (apply actor-constructor become args))
  (match actor-handler
    ;; New procedure, so let's set it
    [(? procedure?)
     (let ((actor-refr
            (make-local-object-refr debug-name vat-connector)))
       (actormap-set! actormap actor-refr
                      (mactor:object actor-handler
                                     become-unseal become?))
       actor-refr)]
    [(? live-refr? pre-existing-refr)
     pre-existing-refr]
    [_
     (error 'invalid-actor-handler "Not a procedure or live refr: ~a" actor-handler)]))

;; These two are user-facing procedures.  Thus, they set up
;; their own syscaller.

;; non-committal version of actormap-spawn
(define (actormap-spawn actormap actor-constructor . args)
  (define new-actormap
    (make-transactormap actormap))
  (call-with-fresh-syscaller
   new-actormap
   (lambda (sys get-sys-internals)
     (define actor-refr
       (actormap-spawn!* new-actormap actor-constructor
                         args))
     (values actor-refr new-actormap))))

(define (actormap-spawn! actormap actor-constructor . args)
  (define new-actormap
    (make-transactormap actormap))
  (define actor-refr
    (call-with-fresh-syscaller
     new-actormap
     (lambda (sys get-sys-internals)
       (actormap-spawn!* new-actormap actor-constructor args))))
  (transactormap-merge! new-actormap)
  actor-refr)

(define* (actormap-spawn-mactor! actormap mactor
                                 #:optional
                                 [debug-name #f])
  (define vat-connector
    (actormap-vat-connector actormap))
  (define actor-refr
    (if (mactor:object? mactor)
        (make-local-object-refr debug-name vat-connector)
        (make-local-promise-refr vat-connector)))
  (actormap-set! actormap actor-refr mactor)
  actor-refr)


;;; actormap turning and utils
;;; ==========================

(define (actormap-turn* actormap to-refr args)
  (call-with-fresh-syscaller
   actormap
   (lambda (sys get-sys-internals)
     (define result-val
       (sys 'S to-refr args))
     (apply values result-val
            (get-sys-internals)))))  ; actormap new-msgs

(define (actormap-turn actormap to-refr . args)
  (define new-actormap
    (make-transactormap actormap))
  (actormap-turn* new-actormap to-refr args))

;; run a turn but only for getting the result.
;; we're not interested in committing the result
;; so we discard everything but the result.
(define (actormap-peek actormap to-refr . args)
  (define-values (returned-val _am _nm)
    (actormap-turn* (make-transactormap actormap)
                    to-refr args))
  returned-val)

;; Note that this does nothing with the messages.
(define (actormap-poke! actormap to-refr . args)
  (define-values (returned-val transactormap _nm)
    (actormap-turn* (make-transactormap actormap)
                    to-refr args))
  (transactormap-merge! transactormap)
  returned-val)

(define (actormap-reckless-poke! actormap to-refr . args)
  (define-values (returned-val transactormap _nm)
    (actormap-turn* actormap to-refr args))
  returned-val)

;; like actormap-run but also returns the new actormap, new-msgs
(define (actormap-run* actormap thunk)
  (define-values (actor-refr new-actormap)
    (actormap-spawn (make-transactormap actormap) (lambda (bcom) thunk)))
  (define-values (returned-val new-actormap2 new-msgs)
    (actormap-turn* (make-transactormap new-actormap) actor-refr '()))
  (values returned-val new-actormap2 new-msgs))

;; non-committal version of actormap-run
(define (actormap-run actormap thunk)
  (define-values (returned-val _am _nm)
    (actormap-run* (make-transactormap actormap) thunk))
  returned-val)

;; committal version
;; Run, and also commit the results of, the code in the thunk
(define* (actormap-run! actormap thunk
                        #:key [reckless? #f])
  (define actor-refr
    (actormap-spawn! actormap
                     (lambda (bcom)
                       (lambda ()
                         (call-with-values thunk list)))))
  (define actormap-poker!
    (if reckless?
        actormap-reckless-poke!
        actormap-poke!))
  (apply values (actormap-poker! actormap actor-refr)))
