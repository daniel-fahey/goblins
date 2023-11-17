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

;; STAGE 1: Add:
;;  - mactor:object
;;  - minimal syscaller
;;  - $
;;  - actormap-direct-run!

(define-module (pre-goblins stage1)
  #:export (make-whactormap
            make-actormap

            spawn $

            actormap-direct-run!

            ;;;; yet to come:
            ;; <- <-np on
            )
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match))

(define-record-type <actormap>
  (make-actormap metatype data vat-connector)
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
  *unspecified*)

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
  (make-actormap whactormap-metatype
                 (make-whactormap-data (make-weak-key-hash-table))
                 vat-connector))

(define make-actormap make-whactormap)



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
  (define* (become new-behavior #:optional [return-val *unspecified*])
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
        [($) _$]
        [(spawn) _spawn]
        [(vat-connector) get-vat-connector]
        [(near-refr?) near-refr?]
        [(near-mactor) near-mactor]
        [else (error 'invalid-syscaller-method
                     "~a" method-id)]))
    (apply method args))

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
  (define (_$ to-refr args)
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
                  (with-continuation-barrier
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

  (define (get-internals)
    (values actormap new-msgs))

  (define (close-up!)
    (set! closed? #t))

  (values this-syscaller get-internals close-up!))

(define (call-with-fresh-syscaller am proc)
  (define-values (sys get-sys-internals close-up!)
    (fresh-syscaller am))
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (proc sys get-sys-internals))
    (lambda ()
      (close-up!))))

(define (actormap-direct-run! am thunk)
  (call-with-fresh-syscaller
   am
   (lambda (sys get-sys-internals)
     (parameterize ([current-syscaller sys])
       (thunk)))))

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
(define ($ refr . args)
  (define sys (get-syscaller-or-die))
  (sys '$ refr args))
