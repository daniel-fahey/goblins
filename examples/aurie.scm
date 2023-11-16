(define-module (examples aurie)
  #:use-module ((goblins) #:renamer (lambda (x) (if (eq? x '$) '$C x)))
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins utils define-actor)
  #:export (^cell
            cell-aurenv

            ^greeter
            greeter-aurenv))

;; Macroless
(define (^cell bcom value)
  (define main-beh
    (case-lambda
      [() value]
      [(new-value) (bcom (^cell bcom new-value))]))
  (define (self-portrait)
    (list value))
  (portraitize main-beh self-portrait))

;; With the new `define-actor' macro
(define-actor (^greeter* bcom our-name number-of-times)
  (lambda (their-name)
      (bcom (^greeter* bcom our-name (+ 1 number-of-times))
            (format #f "Hello ~a, my name is ~a (called: ~a)"
                    their-name our-name number-of-times))))

(define (^greeter bcom our-name)
  (^greeter* bcom our-name 0))
(define (restore-greeter our-name number-of-times)
  (spawn ^greeter* our-name number-of-times))

(define cell-aurenv
  (make-aurenv
    (list (make-auriable '((sandbox aurie) ^cell) ^cell))
    (list)))

(define greeter-aurenv
  (make-aurenv
   (list (make-auriable '((sandbox aurie) ^greeter) ^greeter restore-greeter))
   (list)))

(define am
  (make-actormap))

(define alice
  (actormap-run!
   am
   (lambda ()
     (define alice (spawn ^greeter "Alice"))
     (format #t "[Alice says to Bob]\t ~a\n" ($C alice "Bob"))
     (format #t "[Alice says to Carol]\t ~a\n" ($C alice "Carol"))
     alice)))

(define-values (slots->depictions val->slot slot->val root-slots)
  (actormap-take-portrait am greeter-aurenv alice))

(define another-am
  (make-actormap))

(define-values (restored-alice)
  (actormap-restore another-am greeter-aurenv slots->depictions root-slots))

(actormap-run!
 another-am
 (lambda ()
   (format #t "\n== after resturation ==\n")
   (format #t "[Alice says to Jessica]\t ~a\n" ($C restored-alice "Jessica"))))
