(define-module (goblins contrib aurie)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 q)
  #:use-module (ice-9 match)
  #:use-module ((goblins core) #:renamer (lambda (x) (if (eq? x '$) '$C x)))
  #:use-module (goblins ghash)
  #:use-module (goblins utils simple-sealers)
  #:use-module (goblins contrib syrup)
  #:use-module (ice-9 iconv)
  #:use-module (rnrs bytevectors)
  #:export (make-depict
            depict-manager-add!
            depict-manager-ez-add!
            get-depictions
            restore-from-depictions))

(define-record-type <buildable-depiction>
  (make-buildable-depiction brand? sealed-args)
  buildable-depiction?
  (brand? buildable-depiction-brand?)
  (sealed-args buildable-depiction-sealed-args))

(define-record-type <depict-manager>
  (_make-depict-manager brands->unbuilders names->builders)
  depict-manager?
  (brands->unbuilders depict-manager-brands->unbuilders)
  (names->builders depict-manager-names->builders))

(define-record-type <unbuilder>
  (make-unbuilder unsealer name phase)
  unbuilder?
  (unsealer unbuilder-unsealer)
  (name unbuilder-name)
  (phase unbuilder-phase))

(define-record-type <builder>
  (make-builder rebuilder phase)
  builder?
  (rebuilder builder-rebuilder)
  (phase builder-phase))


(define (enforce-brand brand? sealed-depiction)
  (unless (brand? sealed-depiction)
    (error "sealed depiction does not match brand? predicate")))

(define (make-depict seal brand?)
  (lambda args
    (define sealed-depict (seal args))
    (make-buildable-depiction brand? sealed-depict)))

(define (hash-has-key? hash key)
  (not (eq? (hash-ref hash key #f) #f)))

(define (make-depict-manager)
  ;; Return depict-manager struct???
  (_make-depict-manager (make-hash-table) (make-hash-table)))

(define* (depict-manager-add! depict-mgr name unsealer brand? rebuilder #:key (builder-phase 'outer))
  (define-values (brands->unbuilders names->builders)
    (values (depict-manager-brands->unbuilders depict-mgr)
            (depict-manager-names->builders depict-mgr)))

  (when (hash-has-key? brands->unbuilders brand?)
    (error "Depict manager already has brand:" brand?))
  (when (hash-has-key? names->builders name)
    (error "Depict manager already has name:" name))
  (hash-set! brands->unbuilders brand? (make-unbuilder unsealer name builder-phase))
  (hash-set! names->builders name (make-builder rebuilder builder-phase)))

(define (atom? obj)
  (or (bytevector? obj) (number? obj) (string? obj)
      (symbol? obj) (boolean? obj)))

(define* (get-depictions depict-mgr portrait-incanter #:key (allow-broken? #f) . roots)
  (define process-queue
    (make-q))
  (define-values (session-seal session-unseal session-depict?)
    (make-sealer-triplet))

  (define next-id 0)
  (define val->slot
    (make-ghash))
  (define slot->val
    (make-ghash))
  (define slot->depiction
    (make-ghash))

  (define (slot-maybe-queue-near-ref! obj)
    (or (ghash-ref val->slot obj #f)
        (let ([this-slot next-id])
          (set! next-id (+ 1 next-id))
          (set! val->slot (ghash-set val->slot obj this-slot))
          (set! slot->val (ghash-set slot->val this-slot obj))
          (enq! process-queue obj)
          this-slot)))

  (define root-slots
    (map slot-maybe-queue-near-ref! roots))

  ;; TODO: handle allow-broken?

  (define (read-next-portrait!)
    (define this-obj
      (deq! process-queue))
    ;; well at this point if it isn't queued already we're in trouble
    (define slot
      (ghash-ref val->slot this-obj))

    (define (process-buildable-depiction buildable-depiction phase)
      (match buildable-depiction
        [($ <buildable-depiction> brand? sealed-depiction)
         ;; Don't know of how this could happen at this point but best
         ;; to be extra careful
         (enforce-brand brand? sealed-depiction)

         (define-values (unseal-portrait build-name unbuilder-phase)
           (match (hash-ref (depict-manager-brands->unbuilders depict-mgr) brand?)
             [($ <unbuilder> unsealer name phase)
              (values unsealer name phase)]))

         (unless (eq? unbuilder-phase phase)
           (error "Trying to unbuild depiction in wrong phase" phase))

         (define unsealed-depiction
           (unseal-portrait sealed-depiction))
         (define processed-unsealed-depiction
           (map process-one  unsealed-depiction))

         (make-syrec* 'build build-name processed-unsealed-depiction)]))

    (define (process-one obj)
      (match obj
        [(? atom?) obj]
        [(? list?)
         (make-syrec 'list (map process-one obj))]
        [(? vector?)
         (make-syrec 'vector (map process-one (vector->list obj)))]
        [(? ghash?)
         (ghash-fold
          (lambda (key value prev)
            (ghash-set prev (process-one key) (process-one value)))
          (make-ghash)
          obj)]
        [(? set?)
         (set-fold
          (lambda (val prev)
            (set-add prev (process-one val)))
          (make-set)
          obj)]
        [(? keyword?)
         (make-syrec* 'keyword (keyword->symbol obj))]
        [(? local-object-refr?)
         (make-syrec* 'near-refr (slot-maybe-queue-near-ref! obj))]
        [(? local-promise-refr?)
         (unless (near-promise-settled? obj)
           (error "Can't serialize unsettled promise: " obj))
         (process-one (near-settled-promise-value obj))]
        [(? buildable-depiction?)
         (process-buildable-depiction obj 'inner)]))

    (define returned-depiction
      (session-unseal ($C portrait-incanter this-obj session-seal)))

    (define depiction-to-save
      (process-buildable-depiction returned-depiction 'outer))

    (set! slot->depiction (ghash-set slot->depiction slot depiction-to-save)))

  ;; Blah not `while`?
  (let lp ()
    (read-next-portrait!)
    (unless (q-empty? process-queue)
      (lp)))
  (values slot->depiction val->slot slot->val root-slots))

(define* (restore-from-depictions depict-mgr slots->depictions #:optional (root-slot-or-slots 0))
  (define slots->promises
    (make-ghash))
  (define slots->resolvers
    (make-ghash))

  (define names->builders
    (depict-manager-names->builders depict-mgr))

  (ghash-for-each
   (lambda (slot _depiction)
     (define-values (vow resolver)
       (spawn-promise-values))
     (set! slots->promises (ghash-set slots->promises slot vow))
     (set! slots->resolvers (ghash-set slots->resolvers slot resolver)))
   slots->depictions)

  ;; The question is, which order to restore in?
  (define (restore-slot! slot depiction)
    (define resolver
      (ghash-ref slots->resolvers slot))

    (define (restore-buildable depicted phase)
      (match depicted
        [($ <syrec> 'build build-params)
         (define-values (build-name depict-args)
           (values (car build-params) (car (cdr build-params))))
         (define-values (rebuilder builder-phase)
           (match (hash-ref names->builders build-name)
             [($ <builder> rebuilder builder-phase)
              (values rebuilder builder-phase)]))
         (unless (eq? builder-phase phase)
           (error "Trying to rebuild depiction in wrong phase" phase))
         (define args
           (map restore-one depict-args))
         (apply rebuilder args)]))

    (define (restore-one depicted)
      (match depicted
        [(? atom? val) val]
        [($ <syrec> 'list args)
         (map restore-one args)]
        [($ <syrec> 'vector args)
         (list->vector (map restore-one args))]
        [($ <syrec> 'keyword kw)
         (symbol->keyword (car kw))]
        [(? ghash?)
         (ghash-fold
          (lambda (key value prev)
            (ghash-set prev (restore-one key) (restore-one value)))
          (make-ghash)
          depicted)]
        [(? set?)
         (error "TODO: fix sets")]
         ;; (let* restore-set ([restored-set (make-set)]
         ;;                    [remaining (set->list depicted)])
         ;;   (if (null? (cdr remaining))
         ;;       (set-add restored-set (restore-one (car remaining)))
         ;;       (restore-set
         ;;        (set-add restored-set (restore-one (car remaining)))
         ;;        (cdr remaining))))]
        [($ <syrec> 'near-refr  depicted-slots)
         (ghash-ref slots->promises (car depicted-slots))]
        [($ <syrec> 'build _build-args)
         (restore-buildable depicted 'inner)]))

    (define restored
      (restore-buildable depiction 'outer))
    ($C resolver 'fulfill restored))

  (ghash-for-each
   (lambda (slot depiction)
     (restore-slot! slot depiction))
   slots->depictions)

  ;; Finally we return the depiction references starting with the root slots
  (match root-slot-or-slots
    [(? list root-slots)
     (map (lambda (slot)
            (ghash-ref slots->promises slot))
          root-slots)]
    [(? integer? slot)
     (ghash-ref slots->promises slot)]))

(define (depict-manager-ez-add! depict-mgr build-name rebuild)
  (define builders->name
    (depict-manager-names->builders depict-mgr))

  (if (hash-has-key? builders->name build-name)
      #f
      (let-values ([(depict-seal depict-unseal depict?) (make-sealer-triplet)]
                    [(depict) (make-depict depict-seal depict?)])
        (depict-manager-add! depict-mgr build-name
                             depict-unseal depict? rebuild)
        depict)))



;;; ---------------
;; Testingggg!!!!!!
; (use-modules (goblins vat)
;              (goblins actor-lib methods)
;              (goblins actor-lib ward))

; (define testing-vat
;   (spawn-vat))

; (define (test-me)
;   (with-vat testing-vat
;     (define depict-mgr
;       (make-depict-manager))

;     (define-values (portrait-warden portrait-incanter)
;       (spawn-warding-pair))

;     (define ward-portrait
;       (warden->ward-proc portrait-warden))

;     (define* (^robot bcom name #:key (hp 0))
;       (define main-beh
;         (methods
;          [(get-hp) hp]
;          [(get-name) name]
;          [(alive?)
;           (> hp 0)]
;          [(attack damage)
;           (bcom (^robot bcom name #:hp (- hp damage)))]))
;       (define (self-portrait session)
;         (session (robot-depict name #:hp hp)))
;       (ward-portrait self-portrait main-beh))

;     (define-values (my-warden my-incanter)
;       (spawn-warding-pair))

;     (define* (robot-rebuild name #:key [hp 0])
;       (spawn ^robot name #:hp hp))

;     (define robot-depict
;       (depict-manager-ez-add! depict-mgr 'robot robot-rebuild))

;     (define* (^arena bcom #:optional [robots '()])
;       (define main-beh
;         (methods
;          [(add-robot robot)
;           (bcom (^arena bcom (cons robot robots)))]
;          [(get-robots)
;           robots]))
;       (define (self-portrait session)
;         (session (arena-depict robots)))
;       (ward-portrait self-portrait main-beh))

;     (define (arena-rebuild robots)
;       (spawn ^arena robots))

;     (define arena-depict
;       (depict-manager-ez-add! depict-mgr 'arena arena-rebuild))

;     (define robot1
;       (spawn ^robot "MegaCrusher3000" #:hp 40))
;     (define robot2
;       (spawn ^robot "ElectroSlicer8451" #:hp 50))
;     (define arena
;       (spawn ^arena (list robot1 robot2)))

;     (define-values (arena-depictions arena-val->slot arena-slot->val arena-root-slots)
;       (get-depictions depict-mgr portrait-incanter arena))

;     (pk 'arena-depictions arena-depictions
;         'arena-val->slot arena-val->slot
;         'arena-slot->val arena-slot->val
;         'arena-root-slots arena-root-slots)

;     (ghash-for-each
;      (lambda (k v)
;        (pk 'k k 'v v))
;      arena-depictions)
;     ;; Syrup serialize
;     (define syrup-data (syrup-encode arena-depictions))
;     (pk 'syrup-data (bytevector->string syrup-data "ASCII"))
;     ;; Store somewhere safe
;     ;; get back that safety
;     (define restored-from-syrup (syrup-decode syrup-data))


;     (define restored-vow
;       (restore-from-depictions depict-mgr restored-from-syrup arena-root-slots))

;     (on (car restored-vow)
;         (lambda (restored)
;           (pk 'restored-arena restored)
;           (pk 'robots ($C restored 'get-robots))))))

; (test-me)
