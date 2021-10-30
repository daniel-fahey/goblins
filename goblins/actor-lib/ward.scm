;;; Copyright 2020-2021 Christine Lemmer-Webber
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

(define-module (goblins actor-lib ward)
  #:use-module (goblins core)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:export (spawn-warding-pair
            ward
            enchant
            warden->ward-proc))

;; This module provides a "warding" mechanism... behind the ward is some
;; interesting behavior an actor might not quite everyone to have access
;; to.  For every warder there is an associated incanter who can break
;; through the magical barrier to access that behavior.
;;
;;  - spawn-warding-pair: returns two values (both actors) to its
;;    continuation, a warden (who protects behavior) and an incanter (who
;;    can poke through the ward to access that behavior).
;;
;;    The warden is not used directly, see `ward` defined below.
;;
;;    The incanter is an actor which is called with its first argument, a
;;    target actor whose warded behavior we want to access, and the
;;    remaining arguments are passed to the 
;; 
;;  - ward: Use WARDEN to protect BEHAVIOR.
;;
;;  - enchant: Set up a proxy object imbued with the powers of INCANTER
;;    to operate on TARGET.  Any messages sent to the returned enchanted
;;    actor will be automatically passed through the incanter to target.
;;
;; See also http://www.erights.org/history/joule/MANUAL.B17.pdf
;; for the inspiration for this pattern

;; A special kind of sealer/unsealer... can only ever be
;; sealed/unsealed once.
(define (make-ward-sealer-triplet)
  (define-record-type <ward-sealed>
    (ward-sealed val shattered?)
    ward-sealed?
    (val ward-sealed-val)
    (shattered? ward-sealed-shattered? set-ward-sealed-shattered?!))
  (define (seal val)
    (ward-sealed val #f))
  (define (unseal sealed)
    (when (ward-sealed-shattered? sealed)
      ;; prevent replay attack
      (error "Seal already shattered!"))
    (set-ward-sealed-shattered?! sealed #t)
    (ward-sealed-val sealed))
  (values seal unseal ward-sealed?))

;; Keyword arguments:
;;  - #:async?: If true, the incanter will use <- instead of $ to
;;    proxy messages sends
;;  - #:sealer-triplet: 
;;
;; Returns two values to its continuation:
;;  - a warden, to be used with the ward procedure below
;;  - an incanter, to access actors which have warded methods
(define* (spawn-warding-pair #:key [async? #f] [sealer-triplet #f])
  (define-values (seal unseal sealed?)
    (match sealer-triplet
      [(seal unseal sealed?)
       (values seal unseal sealed?)]
      [#f
       (make-ward-sealer-triplet)]))
  ;; When invoked, the warden returns either:
  ;;  - #f: if these are not arguments sealed by the sealer, or
  ;;  - (list kws kw-vals args): the unsealed arguments
  (define (^warden _bcom)
    (lambda (maybe-sealed-args)
      (and (sealed? maybe-sealed-args)
           (unseal maybe-sealed-args))))
  (define (^incanter _bcom)
    (define $/<-
      (if async? <- $))
    (lambda (target . args)
      ($/<- target (seal args))))

  (values (spawn ^warden) (spawn ^incanter)))

;; Sets up a magical barrier using the powers of `warden` to restrict
;; access to `behavior`.
;; #:extends, if provided, is a fallback procedure.
;;
;; If #:async? is #t, this will result in any call to this warded
;; behavior will return a promise.
(define* (ward warden behavior
               #:key
               [extends #f]
               [async? #f])
  (define (error-out)
    (error "Not sealed args and no extended behavior"))
  (case-lambda
    [(maybe-sealed-args)
     (let ((after-maybe-unsealed
            (match-lambda
              ;; oh, it's a list?  then it's a match!
              [(unsealed-args ...)
               (apply behavior unsealed-args)]
              ;; Hm, didn't match the seal, so let's use the extended
              ;; methods if appropriate...
              [#f
               (if extends
                   ;; package these up
                   (apply extends (list maybe-sealed-args))
                   (error-out))])))
       (if async?
           (on (<- warden maybe-sealed-args)
               after-maybe-unsealed
               #:promise? #t)
           (after-maybe-unsealed ($ warden maybe-sealed-args))))]
    ;; We still need to make this async if async? is #t
    ;; for consistency... unfortunately this can result in a lot of
    [args
     (let ((apply-or-error
            (lambda _whatever
              (if extends
                  (apply extends args)
                  (error-out)))))
       ;; lazy way to set up and subscribe to an immediately resolved promise
       (if async?
           (on #t apply-or-error #:promise? #t)
           (apply-or-error)))]))

;; This doesn't work right in guile as easily because of the way lambda*
;; lumps all keyword arguments together in case of a rest pattern
;; Re-enable this shugary multi-ward later...
#;(define* (ward #:key [extends #f] [async? #f]
               . warden-behaviors)
  (let lp ([warden-behaviors warden-behaviors])
    (match warden-behaviors
      [(warden behavior rest ...)
       (_ward warden behavior
              #:extends (lp rest)
              #:async? async?)]
      ['() extends])))

(define (warden->ward-proc warden)
  (define (ward-proc warded-beh extends-beh)
    (ward warden warded-beh
          #:extends extends-beh))
  ward-proc)

;; Sets up an "incantified proxy" that always sends messages through
;; the incanter
(define* (^incantified _bcom incanter target
                       #:key [async? #f])
  (define $/<-
    (if async? <- $))
  (lambda args
    (apply $/<- incanter target args)))

(define* (enchant incanter target
                  #:key [async? #f])
  (spawn ^incantified incanter target #:async? async?))
