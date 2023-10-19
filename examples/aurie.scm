(use-modules (goblins)
             (goblins ghash)
             (goblins actor-lib methods)
             (srfi srfi-9)
             (srfi srfi-11))

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

(define (robot-depict name hp)
  (spawn ^robot name #:hp hp))

(define robot-aurenv
  (make-aurenv
    (list (list '((sandbox aurie) ^robot) robot-depict))
    (list)))

(define my-actormap
  (make-actormap))

(define robot1
  (actormap-spawn! my-actormap ^robot "MegaCrusher3000" #:hp 40))

(define robot2
  (actormap-spawn! my-actormap ^robot "ElectroSlicer8451" #:hp 50))

(pk 'robot1 robot1 'robot2 robot2)

(define-values (slots->depictions val->slot slot->val root-slots)
  (actormap-take-portrait my-actormap robot1))

(pk 'slots->depictions slots->depictions
    'val->slot val->slot
    'slot->val slot->val
    'root-slots root-slots)
