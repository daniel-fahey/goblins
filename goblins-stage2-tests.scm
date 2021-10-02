(define-module (goblins tests test-stage2)
  #:use-module (goblins stage2)
  #:use-module (srfi srfi-64))

(test-begin "test-goblins-stage2")

(define am (make-whactormap))

(define (^greeter _bcom my-name)
  (lambda (your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name)))

(define alice
  (actormap-spawn! am ^greeter "Alice"))

(test-equal "Hello Bob, my name is Alice!"
  (actormap-peek am alice "Bob"))

(define (^gregarious _bcom my-name)
  (lambda (talk-to)
    (format #f "I heard back: ~a"
            ($ talk-to my-name))))

(define greg
  (actormap-spawn! am ^gregarious "Greg"))

;; Actors which call other actors
(test-equal "I heard back: Hello Greg, my name is Alice!"
  (actormap-peek am greg alice))

;; Actor updates: update and return value separately
(define* (^cell bcom #:optional [val #f])
  (case-lambda
    [() val]
    [(new-val) (bcom (^cell bcom new-val))]))

(define _void (if #f #f))

(actormap-run!
 am
 (lambda ()
   (define cell (spawn ^cell))
   (test-equal ($ cell) #f)          ; initial val
   (test-equal ($ cell 'foo) _void)  ; update (no return value)
   (test-equal ($ cell) 'foo)))      ; new val

;; Actor updates: update and return value at same time
(define* (^counter bcom #:optional [n 0])
  (lambda ()
    (bcom (^counter bcom (1+ n)) n)))

(actormap-run!
 am
 (lambda ()
   (define ctr (spawn ^counter))
   (test-equal 0 ($ ctr))
   (test-equal 1 ($ ctr))
   (test-equal 2 ($ ctr))
   (test-equal 3 ($ ctr))))

;; Now for some noncommittal stuff.

(test-end "test-goblins-stage2")
