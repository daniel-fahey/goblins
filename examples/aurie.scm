(use-modules (goblins)
             (goblins actor-lib methods))

;; Test code
(define* (^robot bcom name #:key [hp 0])
  (define main-beh
    (methods
     [(get-hp) hp]
     [(get-name) name]
     [(alive?) (> hp 0)]
     [(attack damage)
      (bcom (^robot bcom name #:hp (- hp damage)))]))
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

(define robot-aurenv
  (make-aurenv
    (list (list '((sandbox aurie) ^robot) ^robot)
          (list '((example aurie) ^arena) ^arena))
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
   ($ arena 'add-robot robot2)))

(define-values (slots->depictions val->slot slot->val root-slots)
  (actormap-take-portrait my-actormap arena))

(pk 'slots->depictions slots->depictions
    'val->slot val->slot
    'slot->val slot->val
    'root-slots root-slots)
(hash-for-each
 (lambda (k v)
   (pk 'key k 'value v))
 slots->depictions)
