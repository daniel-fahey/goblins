;; STAGE 0: Just actormaps that can be spawned, peeked at.
;;   Nothing else... no turns, $, <-, on, or even bcom.

(define-module (goblins stage0)
  #:export ()
  #:use-module (srfi srfi-9))

;; Old hack to get the "unspecified/undefined type"
(define _void (if #f #f))

(define-record-type <actormap>
  (make-actormap metatype data vat-connector)
  actormap?
  (metatype actormap-metatype)
  (data actormap-data)
  (vat-connector actormap-vat-connector))

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
  (hashq-ref wht key))

(define (whactormap-set! am key val)
  (define wht (whactormap-data-wht (actormap-data am)))
  (hashq-set! wht key val))

(define whactormap-metatype
  (make-actormap-metatype 'whactormap whactormap-ref whactormap-set!))

(define* (make-whactormap #:key [vat-connector #f])
  (make-actormap whactormap-metatype
                 (make-whactormap-data (make-weak-key-hash-table))
                 vat-connector))

;; Ref(r)s
;; =======

(define-record-type <local-object-refr>
  (make-local-object-refr debug-name vat-connector)
  refr?
  (debug-name local-object-refr-debug-name)
  (vat-connector local-object-refr-vat-connector))



;; Pre-turn operations, not composable
;; ===================================

(define (actormap-spawn! am constructor . args)
  (define refr
    (make-local-object-refr (procedure-name constructor)
                            #f))
  (define initial-handler
    (apply constructor 'fake-bcom args))
  (actormap-set! am refr initial-handler)
  refr)

(define (actormap-peek am refr . args)
  (define actor-proc
    (actormap-ref am refr))
  (apply actor-proc args))


;; Test area
;; ---------

(define (_test)
  (define am (make-whactormap))
  (define (^greeter _bcom my-name)
    (lambda (your-name)
      (format #f "Hello ~a, my name is ~a!" your-name my-name)))
  (define alice
    (actormap-spawn! am ^greeter "Alice"))
  (display (actormap-peek am alice "Bob"))(newline))
