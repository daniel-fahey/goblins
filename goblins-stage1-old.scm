(define-module (goblins stage1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:export (make-actormap
            actormap-turn

            ;; utils
            actormap-spawn
            actormap-spawn!
            actormap-peek
            actormap-poke!

            
            ))

;; =====
;; Refrs
;; =====
(define-class <local-refr> ()
  (vat-connector #:getter local-refr-vat-connector))
(define-class <local-object-refr> (<local-refr>)
  (debug-name #:getter local-object-refr-debug-name))
(define-class <local-promise-refr> (<local-refr>))

(define* (make-local-object-refr #:key [debug-name #f]
                                 [vat-connector #f])
  (make <local-object-refr>
    #:vat-connector vat-connector
    #:debug-name debug-name))


;; =========
;; Actormaps
;; =========

(define-class <actormap> ()
  (vat-connector #:getter actormap-vat-connector
                 #:init-keyword #:vat-connector))

;; It doesn't look like Guile has ephemerons so I'm a bit nervous GC
;; stuff isn't going to work quite right
(define-class <whactormap> (<actormap>)
  (wht #:getter whactormap-wht
       #:init-keyword #:wht))
(define-class <transactormap> (<actormap>)
  (parent #:getter transactormap-parent
          #:init-keyword #:parent)
  (delta #:getter transactormap-delta
         #:init-keyword #:delta)
  (merged? #:getter transactormap-merged?
           #:setter set-transactormap-merged?!
           #:init-value #f
           #:init-keyword #:merged?))

(define* (make-whactormap #:key
                          [wht (make-weak-key-hash-table)]
                          [vat-connector #f])
  (make <whactormap>
    #:wht wht
    #:vat-connector vat-connector))
(define make-actormap make-whactormap) ; alias

(define (make-transactormap parent)
  (make <transactormap>
    #:parent parent
    #:vat-connector (actormap-vat-connector parent)))

;; If no default is provided, we default to #f
(define-method (actormap-ref (actormap <actormap>) key)
  (actormap-ref actormap key #f))

(define-method (actormap-ref (actormap <whactormap>) key dflt)
  (hashq-ref (whactormap-wht actormap) key dflt))
(define-method (actormap-set! (actormap <whactormap>) key val)
  (hashq-set! (whactormap-wht actormap) key val))

(define *nothing* (list '*nothing*))
(define void (if #f #f))

(define-method (actormap-ref (actormap <transactormap>) key dflt)
  (when (transactormap-merged? actormap)
    (error "Can't use transactormap-ref on merged transactormap"))
  (define tm-delta
    (transactormap-delta actormap))
  (let ((result (hashq-ref tm-delta key *nothing*)))
    (if (not (eq? result *nothing*))
        ;; we got it
        result
        ;; search parents for key
        (let ([parent (transactormap-parent actormap)])
          (actormap-ref parent key dflt)))))
(define-method (actormap-set! (actormap <transactormap>) key val)
  (when (transactormap-merged? actormap)
    (error "Can't use transactormap-set! on merged transactormap"))
  (hashq-set! (transactormap-delta actormap)
              key val)
  void)

(define (transactormap-merge! transactormap)
  'TODO)

(define* (make-whactormap #:key [vat-connector #f])
  (make <whactormap>
    #:vat-connector vat-connector
    #:wht (make-weak-key-hash-table)))

;; alias
(define make-actormap make-whactormap)


(define (run-tests)
  'TODO)


;; =============
;; The syscaller
;; =============

;; NEVER export this.  That would break our security paradigm.
(define current-syscaller
  (make-parameter #f))

(define (fresh-syscaller actormap)
  'TODO)

(define (call-with-fresh-syscaller actormap proc)
  (define-values (sys get-sys-internals close-up!)
    (fresh-syscaller actormap))
  (dynamic-wind
    (lambda () 'no-op)
    (lambda ()
      (parameterize ([current-syscaller sys])
        (proc sys get-sys-internals)))
    (lambda ()
      (close-up!))))


;; =================
;; The Actormap Turn
;; =================

(define (actormap-turn! actormap proc)
  "Does a turn but against the actormap directly.

Returns two values to its continuation:
  (values result-val new-msgs)"
  (call-with-fresh-syscaller
   actormap
   (lambda (sys get-new-msgs)
     (define result-val
       (proc))
     (define new-msgs
       (get-new-msgs))
     (values result-val new-msgs))))

(define (actormap-turn actormap proc)
  (define new-actormap
    (make-transactormap actormap))
  (define-values (result-val new-msgs)
    (actormap-turn! new-actormap proc))
  (values result-val new-actormap new-msgs))

;; TODO: A version of actormap-turn! that sends any outgoing messages,
;; but only if appropriate


;; =============================
;; Actormap Manipulation Helpers
;; =============================


