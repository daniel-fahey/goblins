;; STAGE 2: Add:
;;  - transactormaps
;;  - actormap-poke!
;;  - actormap-peek

(define-module (goblins stage2)
  #:export (make-whactormap
            make-actormap

            spawn $

            actormap-spawn!
            ;; actormap-spawn-mactor!

            actormap-turn*
            actormap-turn

            actormap-peek
            actormap-poke!
            actormap-reckless-poke!

            ;;;; yet to come:
            ;; <- <-np on
            )
  #:use-module (srfi srfi-9)
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
  (hash-set! tm-delta key val)
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

(define-record-type <local-promise-refr>
  (make-local-promise-refr vat-connector)
  local-promise-refr?
  (vat-connector local-promise-refr-vat-connector))

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

#;(define (actormap-poke! am refr . args)
  'TODO)

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
        ['spawn-mactor spawn-mactor]
        ;; TODO:
        ;; ['fulfill-promise fulfill-promise]
        ;; ['break-promise break-promise]
        ;; ;; TODO: These are all variants of 'send-message.
        ;; ;;   Shouldn't we collapse them?
        ;; ['<-np _<-np]
        ;; ['<- _<-]
        ;; ['send-message _send-message]
        ;; ['handle-message _handle-message]
        ;; ['handle-listen _handle-listen]
        ;; ['send-listen _send-listen]
        ;; ['on _on]
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
      ;; ;; If it's an encased value, "calling" it just returns the
      ;; ;; internal value.
      ;; [(? mactor:encased?)
      ;;  (mactor:encased-val mactor)]
      ;; ;; Ah... we're linking to another actor locally, so let's
      ;; ;; just de-symlink and call that instead.
      ;; [(? mactor:local-link?)
      ;;  (keyword-apply _$ kws kw-vals
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

  #;(define (fulfill-promise promise-id sealed-val)
    (call/ec
     (lambda (return-early)
       (define orig-mactor
         (actormap-ref-or-die promise-id))
       (unless (mactor:unresolved? orig-mactor)
         (error 'resolving-resolved
                "Attempt to resolve resolved actor: ~a" promise-id))
       (define resolve-to-val
         (unseal-mactor-resolution orig-mactor sealed-val))

       (define orig-waiting-messages
         (match orig-mactor
           [(? mactor:naive?)
            (mactor:naive-waiting-messages orig-mactor)]
           [(? mactor:closer?)
            (mactor:closer-waiting-messages orig-mactor)]
           [_ '()]))

       (define (forward-messages [waiting-messages orig-waiting-messages])
         (match waiting-messages
           ['() (void)]
           [(list (message _old-to resolve-me kws kw-vals args)
                  rest-waiting ...)
            ;; preserve FIFO by recursing first
            (forward-messages rest-waiting)
            ;; shouldn't be a question message so we don't need to
            ;; #:answer-this-question, I think?
            (_send-message kws kw-vals resolve-to-val resolve-me args)]))

       (define new-waiting-messages
         (if (remote-promise-refr? resolve-to-val)
             ;; don't forward waiting messages to remote promises
             orig-waiting-messages
             ;; but do forward to literally anything else... empty
             ;; the queue!
             (begin (forward-messages)
                    '())))

       (define orig-listeners
         (mactor:unresolved-listeners orig-mactor))

       (define next-mactor-state
         (match resolve-to-val
           [(? local-object-refr?)
            (when (eq? resolve-to-val promise-id)
              (return-early
               ;; We want to break this because it should be explicitly clear
               ;; to everyone that the promise was broken.
               (break-promise promise-id
                              ;; TODO: we need some sort of error type we do
                              ;;   allow to explicitly be shared, this one is a
                              ;;   reasonable candidate
                              'cycle-in-promise-resolution)))
            (mactor:local-link resolve-to-val)]
           [(? remote-object-refr?)
            ;; Since the captp connection is the one that might break this,
            ;; we need to ask it what it uses as its resolver unsealer/tm
            ;; @@: ... This doesn't seem like a good solution.
            ;;   Whatever, we need to add when-broken or something.
            (match-define (cons new-resolver-unsealer new-resolver-tm?)
              (let ([connector (remote-refr-captp-connector resolve-to-val)])
                ;; TODO: Do we need to notify it that we want to know about
                ;;   breakage?  Presumably... so do it here instead...?
                (connector 'partition-unsealer-tm-cons)))

            (mactor:remote-link new-resolver-unsealer new-resolver-tm?
                                resolve-to-val)]
           [(or (? local-promise-refr?)
                (? remote-promise-refr?))
            (define new-history
              (if (mactor:closer? orig-mactor)
                  (set-add (mactor:closer-history orig-mactor)
                           (mactor:closer-point-to orig-mactor))
                  (seteq promise-id)))
            ;; Detect cycles!
            (when (set-member? new-history resolve-to-val)
              ;; not sure we actually need to return anything, but I guess
              ;; this is mildly future-proof.
              (return-early
               ;; We want to break this because it should be explicitly clear
               ;; to everyone that the promise was broken.
               (break-promise promise-id
                              ;; TODO: we need some sort of error type we do
                              ;;   allow to explicitly be shared, this one is a
                              ;;   reasonable candidate
                              'cycle-in-promise-resolution)))

            ;; Make a new set of resolver sealers for this.
            ;; However, we don't use the general ^resolver because we're
            ;; explicitly using the fulfilled-handler/broken-handler things
            (define-values (new-resolver-sealer new-resolver-unsealer new-resolver-tm?)
              (make-sealer-triplet 'fulfill-promise))
            (define new-resolver
              (_spawn ^resolver '() '() (list promise-id new-resolver-sealer)))
            ;; Now subscribe to the promise...
            (_send-listen resolve-to-val new-resolver #t)
            ;; Now we want to both inform any listeners that are interested
            ;; in partial information and scrub them out of the current
            ;; listeners list.
            (define new-listeners
              (for/fold ([new-listeners '()]
                         #:result (reverse new-listeners))
                        ([listener-info orig-listeners])
                (if (listener-info-wants-partial? listener-info)
                    ;; resolve and drop out of listeners
                    (begin (_<-np (listener-info-resolve-me listener-info)
                                  'fulfill resolve-to-val)
                           new-listeners)
                    (cons listener-info new-listeners))))
            ;; Now we become "closer" to this promise
            (mactor:closer new-resolver-unsealer new-resolver-tm?
                           new-listeners
                           resolve-to-val new-history
                           new-waiting-messages)]
           ;; anything else is an encased value
           [_ (mactor:encased resolve-to-val)]))

       ;;  - Now actually switch to the new mactor state
       (actormap-set! actormap promise-id
                      next-mactor-state)

       ;; Resolve listeners, if appropriate (ie, if not mactor:closer)
       (unless (mactor:unresolved? next-mactor-state)
         (for ([listener-info orig-listeners])
           (<-np (listener-info-resolve-me listener-info)
                 'fulfill resolve-to-val))))))

  ;; TODO: Add support for broken-because-of-network-partition support
  ;;   even for mactor:remote-link
  #;(define (break-promise promise-id sealed-problem)
    (match (actormap-ref actormap promise-id #f)
      ;; TODO: Not just local-promise, anything that can
      ;;   break
      [(? mactor:unresolved? unresolved-mactor)
       (define problem
         (unseal-mactor-resolution unresolved-mactor sealed-problem))
       ;; Now we "become" broken with that problem
       (actormap-set! actormap promise-id
                      (mactor:broken problem))
       ;; Inform all listeners of the resolution
       (for ([listener-info (in-list (mactor:unresolved-listeners unresolved-mactor))])
         (<-np (listener-info-resolve-me listener-info)
               'break problem))]
      [(? mactor:remote-link?)
       (error "TODO: Implement breaking on captp disconnect!")]
      [#f (error "no actor with this id")]
      [_ (error "can only resolve eventual references")]))


  ;; Note that _handle-message is really, seriously for handling *toplevel*
  ;; messages... ie, turns.
  ;; This is the bulk of what's called and handled by actormap-turn-message.
  ;; (As opposed to actormap-turn*, which only supports calling, this also
  ;; handles any toplevel invocation of an actor, probably via message send.)
  #;(define (_handle-message msg display-or-log-error)
    (match-define (message to-refr resolve-me kws kw-vals args)
      msg)
    (unless (near-refr? to-refr)
      (error 'not-a-near-refr "Not a near refr: ~a" to-refr))

    (define orig-mactor
      (actormap-ref-or-die to-refr))

    ;; Prevent someone trying to throw this vat into an infinite loop
    (when (eq? to-refr resolve-me)
      (error 'same-recipient-and-resolver
             "Recipient and resolver are the same: ~a" to-refr))

    (define (call-with-resolution proc)
      (with-handlers ([exn:fail?
                       (lambda (err)
                         (when display-or-log-error
                           (display-or-log-error err))
                         ;; We need to revert any messages that were going
                         ;; to send to preserve transactionality
                         (set! new-msgs '())
                         ;; ... but we're still going to send this one
                         (when resolve-me
                           (_<-np resolve-me 'break err))
                         `#(fail ,err))])
        (define call-result
          (proc))
        (when resolve-me
          (_<-np resolve-me 'fulfill call-result))
        `#(success ,call-result)))

    (match orig-mactor
      ;; If it's callable, we just use the call behavior, because
      ;; that's effectively the same code we'd be running anyway.
      ;; However, we do want to handle the resolution.
      [(or (? mactor:object?)
           (? mactor:encased?))
       (call-with-resolution
        (lambda () (keyword-apply _$ kws kw-vals to-refr args)))]
      [(mactor:local-link point-to)
       (cond
         [(near-refr? point-to)
          (call-with-resolution
           (lambda () (keyword-apply _$ kws kw-vals point-to args)))]
         ;; it's not near so we need to pass this along
         [else
          (_send-message kws kw-vals point-to resolve-me args)
          `#(success ,(void))])]
      [(mactor:broken problem)
       (_<-np resolve-me 'break problem)
       `#(fail ,problem)]
      [(? mactor:remote-link?)
       (define point-to (mactor:remote-link-point-to orig-mactor))
       (call-with-resolution
        (lambda ()
          ;; Pass along the message
          ;; Mild optimization: only produce a promise if we have a resolver
          (keyword-apply (if resolve-me
                             _<-
                             _<-np)
                         kws kw-vals point-to args)))]
      ;; Messages sent to a promise that is "closer" are a kind of
      ;; intermediate state; we build a queue.
      [(mactor:closer resolver-unsealer resolver-tm?
                      listeners
                      point-to history
                      waiting-messages)
       (match point-to
         ;; If we're pointing at another near promise then we recurse
         ;; to _handle-messages with the next promise...
         [(? local-promise-refr?)
          ;; Now we need to see if it's in the same vat...
          (cond
            [(near-refr? point-to)
             ;; (We don't use call-with-resolution because the next one will!)
             (_handle-message (message point-to resolve-me kws kw-vals args)
                              display-or-log-error)]
            [else
             ;; Otherwise, we need to forward this message to the appropriate
             ;; vat
             (_send-message kws kw-vals point-to resolve-me args)
             `#(success ,(void))])]
         ;; But if it's a remote promise then we queue it in the waiting
         ;; messages because we prefer to have messages "swim as close
         ;; as possible to the machine barrier where possible", with
         ;; the exception of questions/answers which always cross over
         ;; (see mactor:question handling later in this procedure)
         [(? remote-promise-refr?)
          ;; Since we're queueing to send the message until it resolves
          ;; we don't resolve the problem here... hence we don't
          ;; use call-with-resolution here either.
          (actormap-set! actormap to-refr
                         (mactor:closer resolver-unsealer resolver-tm?
                                        listeners
                                        point-to history
                                        (cons msg waiting-messages)))
          ;; But we should return that this was deferred
          '#(deferred ,(void))])]
      ;; Similar to the above w/ remote promises, except that we really
      ;; just don't know where things go *at all* yet, so no swimming
      ;; occurs.
      [(mactor:naive resolver-unsealer resolver-tm?
                     listeners waiting-messages)
       (actormap-set! actormap to-refr
                      (mactor:naive resolver-unsealer resolver-tm?
                                    listeners
                                    (cons msg waiting-messages)))
       `#(deferred ,(void))]
      ;; Questions should forward their messages to the captp thread
      ;; to deal with using the relevant question-finder.
      [(? mactor:question?)
       (call-with-resolution
        (lambda ()
          (define to-question-finder
            (mactor:question-question-finder orig-mactor))
          (define captp-connector
            (mactor:question-captp-connector orig-mactor))
          (cond
            ;; If we're being asked to resolve something, this is a
            ;; "followup question"
            [resolve-me
             (define followup-question-finder
               (captp-connector 'new-question-finder))
             (define-values (followup-question-promise followup-question-resolver)
               (_spawn-promise-values #:question-finder
                                      followup-question-finder
                                      #:captp-connector
                                      captp-connector))
             (captp-connector
              'handle-message
              (question-message to-question-finder followup-question-resolver
                                kws kw-vals args
                                followup-question-finder))
             followup-question-promise]
            ;; Otherwise, we can just send it without any question and return
            ;; void
            [else
             (captp-connector
              'handle-message
              (message to-question-finder #f
                       kws kw-vals args))
             (void)])))]))

  ;; helper to the below two methods
  #;(define (_send-message kws kw-vals to-refr resolve-me args
                         #:answer-this-question [answer-this-question #f])
    (unless (live-refr? to-refr)
      (error 'send-message
             "Don't know how to send a message to: ~a" to-refr))
    (define new-message
      (if answer-this-question
          (question-message to-refr resolve-me kws kw-vals args
                            answer-this-question)
          (message to-refr resolve-me kws kw-vals args)))
    (set! new-msgs (cons new-message new-msgs)))

  #;(define _<-np
    (make-keyword-procedure
     (lambda (kws kw-vals to-refr . args)
       (_send-message kws kw-vals to-refr #f args)
       (void))))

  #;(define _<-
    (make-keyword-procedure
     (lambda (kws kw-vals to-refr . args)
       (match to-refr
         [(? local-refr?)
          (define-values (promise resolver)
            (_spawn-promise-values))
          (_send-message kws kw-vals to-refr resolver args)
          promise]
         [(? remote-refr?)
          (define captp-connector
            (remote-refr-captp-connector to-refr))
          (define question-finder
            (captp-connector 'new-question-finder))
          (define-values (promise resolver)
            (_spawn-promise-values #:question-finder
                                   question-finder
                                   #:captp-connector
                                   captp-connector))
          (_send-message kws kw-vals to-refr resolver args
                         #:answer-this-question question-finder)
          promise]
         [to-refr
          (error 'send-message
                 "Don't know how to send a message to: ~a" to-refr)]))))

  #;(define (_send-listen to-refr listener [wants-partial? #f])
    (match to-refr
      [(? live-refr?)
       (define listen-req
         (listen-request to-refr listener wants-partial?))
       (set! new-msgs (cons listen-req new-msgs))]
      [val (<-np listener 'fulfill val)]))

  #;(define (_handle-listen to-refr listener wants-partial? display-or-log-error)
    (with-handlers ([exn:fail?
                     (lambda (err)
                       (when display-or-log-error
                         (display-or-log-error err while-handling-listen-header))
                       `#(fail ,err))])
      (unless (near-refr? to-refr)
        (error 'not-a-near-refr "Not a near refr: ~a" to-refr))
      (define mactor
        (actormap-ref-or-die to-refr))
      (match mactor
        [(? mactor:local-link?)
         (define point-to
           (mactor:local-link-point-to mactor))
         (if (near-refr? point-to)
             (_handle-listen (mactor:local-link-point-to mactor)
                             listener wants-partial? display-or-log-error)
             (_send-listen point-to listener wants-partial?))]
        ;; This object is a local promise, so we should handle it.
        [(? mactor:unresolved?)
         ;; Set a new version of the local-promise with this
         ;; object as a listener
         (actormap-set! actormap to-refr
                        (mactor:unresolved-add-listener mactor listener
                                                        wants-partial?))]
        ;; In the following cases we can resolve the listener immediately...
        [(? mactor:broken? mactor)
         (_<-np listener 'break (mactor:broken-problem mactor))]
        [(? mactor:encased? mactor)
         (_<-np listener 'fulfill (mactor:encased-val mactor))]
        [(? mactor:object? mactor)
         (_<-np listener 'fulfill to-refr)]
        ;; For remote links, we resolve directly to that reference
        [(? mactor:remote-link? mactor)
         (_<-np listener 'fulfill (mactor:remote-link-point-to mactor))])
      ;; return with same semantics that _handle-message does
      `#(success ,(void))))

  ;; At THIS stage, fulfilled-handler, broken-handler, finally-handler should
  ;; be actors or #f.  That's not the case in the user-facing
  ;; `on' procedure.
  #;(define (_on on-refr [fulfilled-handler #f]
               #:catch [broken-handler #f]
               #:finally [finally-handler #f]
               #:promise? [promise? #f])
    (define-values (return-promise return-p-resolver)
      (if promise?
          (spawn-promise-values)
          (values #f #f)))

    ;; These two procedures are called once the fulfillment
    ;; or break of the on-refr has actually occurred.
    (define ((handle-resolution on-resolution
                                resolve-fulfill-command) val)
      (cond [on-resolution
             ;; We can't use _send-message directly, because this may
             ;; be in a separate syscaller at the time it's resolved.
             (define syscaller (get-syscaller-or-die))
             ;; But anyway, we want to resolve the return-p-resolver with
             ;; whatever the on-resolution is, which is why we do this goofier
             ;; roundabout
             (syscaller 'send-message
                        '() '() on-resolution
                        ;; Which may be #f!
                        return-p-resolver
                        (list val))
             (when finally-handler
               (<-np finally-handler))]
            ;; There's no on-resolution, which means we can just fulfill
            ;; the promise immediately!
            [else
             (when finally-handler
               (<-np finally-handler))
             (when return-p-resolver
               (<-np return-p-resolver resolve-fulfill-command val))]))
    (define handle-fulfilled
      (handle-resolution fulfilled-handler 'fulfill))
    (define handle-broken
      (handle-resolution broken-handler 'break))

    ;; The purpose of this listener is that the promise
    ;; *hasn't resolved yet*.  Because of that we need to
    ;; queue something to happen *once* it resolves.
    (define (^on-listener bcom)
      (match-lambda*
        [(list 'fulfill val)
         (handle-fulfilled val)
         (void)]
        [(list 'break problem)
         (handle-broken problem)
         (void)]))
    (define listener
      (_spawn ^on-listener '() '() '()))
    (_send-listen on-refr listener)
    (when promise?
      return-promise))

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
      (proc sys get-sys-internals))
    (lambda ()
      (close-up!))))

;; In case you want to spawn PROC right off of your vat without
;; involving the syscaller at all
#;(define (syscaller-free-thread proc)
  (parameterize ((current-syscaller #f))
    (thread proc)))

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

#;(define (_spawn-promise-values #:key
                               (question-finder #f)
                               (captp-connector #f))
  'TODO)

(define (spawn-promise-values)
  'TODO)
(define (spawn-promise-cons)
  'TODO)


;; ;; (define am (make-whactormap))

;; ;; (actormap-set! am 'hello 'world)


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
       (sys '$ to-refr args))
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


;; Test area
;; ---------

#;(define (_test)
  (define am (make-whactormap))
  (define (^greeter _bcom my-name)
    (lambda (your-name)
      (format #f "Hello ~a, my name is ~a!" your-name my-name)))
  (define alice
    (actormap-spawn! am ^greeter "Alice"))
  (display (actormap-peek am alice "Bob"))(newline))
