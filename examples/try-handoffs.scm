(use-modules (goblins)
             (goblins vat)
             (goblins actor-lib methods)
             (goblins ocapn captp)
             (goblins ocapn ids)
             (goblins ocapn netlayer onion)
             (fibers)
             (fibers channels))

;;; The general gist is to three machines, each of them have a "person"
;;; on them:
;;; - Alice
;;; - Bob
;;; - Carol
;;;
;;; Alice has a reference from bob, alice also has a reference to
;;; carol. Alice wants to introduce Carol to bob.
(define (^alice _bcom bob carol)
  (lambda ()
    (pk 'alice '<-np bob 'greet carol)
    (<- bob carol)))

(define (^bob _bcom)
  (lambda (carol)
    (pk 'bob '<- carol "Hi carol, I'm bob")
    (<- carol "Hi carol, I'm bob")))

(define (^carol bcom)
  (lambda (greeting)
    (pk 'carol 'got greeting)))

;; Spawn bob on b
(define b-vat (spawn-vat #:name "b"))
(define b-onion-netlayer
  (b-vat
   (lambda ()
     (new-onion-netlayer))))
(define b-mycapn
  (b-vat
   (lambda ()
     (spawn-mycapn b-onion-netlayer))))
(define bob
  (b-vat
   (lambda ()
     (spawn ^bob))))
(define bob-sref
  (b-vat
   (lambda ()
     ($ b-mycapn 'register bob 'onion))))

;; Spawn carol on c
(define c-vat (spawn-vat #:name "c"))
(define c-onion-netlayer
  (c-vat
   (lambda ()
     (new-onion-netlayer))))
(define c-mycapn
  (c-vat
   (lambda ()
     (spawn-mycapn c-onion-netlayer))))
(define carol
  (c-vat
   (lambda ()
     (spawn ^carol))))
(define carol-sref
  (c-vat
   (lambda ()
     ($ c-mycapn 'register carol 'onion))))

;; Spawn alice on a
(define a-vat (spawn-vat #:name "a"))
(define a-onion-netlayer
  (a-vat
   (lambda ()
     (new-onion-netlayer))))
(define a-mycapn
  (a-vat
   (lambda ()
     (spawn-mycapn a-onion-netlayer))))
(define alice
  (a-vat
   (lambda ()
     (define bob-vow
       ($ a-mycapn 'enliven bob-sref))
     (define carol-vow
       ($ a-mycapn 'enliven carol-sref))
     (spawn ^alice bob-vow carol-vow))))

;; Finally do the handoff!
(display "This will take a long time, be patient!\n")
(define result-ch (make-channel))
(a-vat
 (lambda ()
   (on ($ alice)
       (lambda (result)
         (syscaller-free-fiber
          (lambda ()
            (put-message result-ch result)))
         result))))

(run-fibers
 (lambda ()
   (pk 'we-heard-back (get-message result-ch))))
