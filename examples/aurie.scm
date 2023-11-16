(use-modules ((goblins) #:renamer (lambda (x) (if (eq? x '$) '$C x)))
             (goblins actor-lib methods)
             (ice-9 match))

;; TODO: move me someplace else
(define (actormap-restore am aurenv depictions root-slot-or-slots)
  (define slots->promises
    (make-hash-table))
  (define slots->resolvers
    (make-hash-table))

  (hash-for-each
   (lambda (slot _depiction)
     (let* ([promise-pair (actormap-run! am spawn-promise-cons)]
            [vow (car promise-pair)]
            [resolver (cdr promise-pair)])
       (hashq-set! slots->promises slot vow)
       (hashq-set! slots->resolvers slot resolver)))
   depictions)

    (define (restore-slot! slot depiction)
      (define resolver
        (hashq-ref slots->resolvers slot))
      (define obj-name
        (car depiction))
      (define obj-depiction
        (cadr depiction))
      (define processed-args
        (map process-one obj-depiction))
      (define-values (auriable obj-aurenv)
        (aurenv-ref aurenv obj-name))
      (define depictor
        (auriable-depictor auriable))

      (define (process-one value)
        (match value
          [($ <depiction> 'near-refr refr-slot)
           (hashq-ref slots->promises refr-slot)]
          [(? list?)
           (map process-one value)]
          [_ value]))

      (actormap-run!
       am
       (lambda ()
         (define restored-obj
           (apply depictor processed-args))
         ($C resolver 'fulfill restored-obj))))

    ;; Restore all the objects in the depictions
    (hash-for-each
     (lambda (slot depiction)
       (restore-slot! slot (depiction-data depiction)))
     slots->depictions)

    (match root-slot-or-slots
      [(? list? root-slots)
       (define restored-roots
         (map (lambda (slot)
              (hashq-ref slots->promises slot))
            root-slots))
       (apply values restored-roots)]
      [(? integer? slot)
       (hashq-ref slots->promises slot)]))


;; Test code
(define* (^robot bcom name #:key [hp 0])
  (define main-beh
    (methods
     [(get-hp) hp]
     [(get-name) name]
     [(alive?) (> hp 0)]
     [(attack damage)
      (bcom (^robot name #:hp (- hp damage)))]))
  (define (self-portrait session)
    (session (list name #:hp hp)))

  (portraitize main-beh self-portrait))

(define* (^arena bcom #:optional [robots '()])
  (define main-beh
    (methods
     ([add-robot robot]
      (bcom (^arena bcom (cons robot robots))))
     ([get-robots] robots)))

  (define (self-portrait session)
    (session (list robots)))
  (portraitize main-beh self-portrait))

;; Just to see we get here.
(define (robot-depict . args)
  (pk 'got-to-robot-depict args)
  (apply spawn ^robot args))

(define robot-aurenv
  (make-aurenv
    (list (make-auriable '((sandbox aurie) ^robot) ^robot robot-depict)
          (make-auriable '((example aurie) ^arena) ^arena))
    (list)))

(define my-actormap
  (make-actormap))

(define robot1
  (actormap-spawn! my-actormap ^robot "MegaCrusher3000" #:hp 40))

(define robot2
  (actormap-spawn! my-actormap ^robot "ElectroSlicer8451" #:hp 50))

(define arena
  (actormap-spawn! my-actormap ^arena (list robot1)))

(actormap-run!
 my-actormap
 (lambda ()
   ($C arena 'add-robot robot2)))

(define-values (slots->depictions val->slot slot->val root-slots)
  (actormap-take-portrait my-actormap robot-aurenv arena))

(define (pp-hash name hash)
  (format #t "# ~a:\n" name)
  (hash-for-each
   (lambda (k v)
     (pk 'key k 'value v))
   hash)
  (format #t "\n"))

(pk 'slots->depictions slots->depictions
    'val->slot val->slot
    'slot->val slot->val
    'root-slots root-slots)

(pp-hash "slots->depictions" slots->depictions)
(pp-hash "val->slot" val->slot)
(pp-hash "slot->val" slot->val)
(pk 'root-slots root-slots)

(define another-am
  (make-actormap))

(define-values (restored-arena)
  (actormap-restore another-am robot-aurenv slots->depictions root-slots))

(define robot1
  (actormap-run!
   another-am
   (lambda ()
     (pk 'robots ($C restored-arena 'get-robots))
     (define robot1 (car ($C restored-arena 'get-robots)))
     (pk 'robot1-name ($C robot1 'get-name)
         'robot1-hp ($C robot1 'get-hp))
     robot1)))

(define* (^new-robot bcom name #:key [hp 0])
  (define main-beh
    (methods
     [(get-hp) hp]
     [(get-name) (format #f "robo ~a" name)]
     [(alive?) (> hp 0)]
     [(attack damage)
      (bcom (^robot name #:hp (- hp damage)))]))
  (define (self-portrait session)
    (session (list name #:hp hp)))

  (portraitize main-beh self-portrait))

;; TODO: write a function to update a constructor in an aurenv
(define new-robot-aurenv
  (make-aurenv
    (list (make-auriable '((sandbox aurie) ^robot) ^new-robot)
          (make-auriable '((example aurie) ^arena) ^arena))
    (list)))

(actormap-replace-behavior another-am robot-aurenv new-robot-aurenv)

(actormap-run
 another-am
 (lambda ()
   (pk 'name ($C robot1 'get-name))))
