(use-modules (goblins)
             (goblins vat)
             (goblins ocapn ids)
             (goblins ocapn captp)
             (goblins ocapn netlayer fake)
             (tests utils)
             (fibers channels)
             (srfi srfi-64))

(test-begin "test-captp")

;; ------------- ;;
;; Handoff test  ;;
;; ------------- ;;
(define (^alice _bcom bob carol)
  (lambda ()
    (<- bob carol)))

(define (^bob _bcom)
  (lambda (carol)
    (<- carol "Hi Carol, I'm Bob")))

(define (^carol bcom)
  (lambda (greeting)
    greeting))

(define test-vat (spawn-vat #:name "test"))
(define test-network
  (test-vat
   (lambda ()
     (spawn ^fake-network))))

(define (make-new-machine name)
  (define machine-vat (spawn-vat #:name name))
  (define new-conn-ch (make-channel))
  (test-vat
   (lambda ()
     ($ test-network 'register name new-conn-ch)))
  (define location (make-ocapn-machine 'fake name #f))
  (define netlayer
    (machine-vat
     (lambda ()
       (spawn ^fake-netlayer name test-network new-conn-ch))))
  (define mycapn
    (machine-vat
     (lambda ()
       (spawn-mycapn netlayer))))
  (values machine-vat netlayer mycapn))

;; Spawn bob on b
(define-values (b-vat b-netlayer b-mycapn)
  (make-new-machine "b"))
(define bob
  (b-vat
   (lambda ()
     (spawn ^bob))))
(define bob-sref
  (b-vat
   (lambda ()
     ($ b-mycapn 'register bob 'fake))))

;; Spawn carol on c
(define-values (c-vat c-netlayer c-mycapn)
  (make-new-machine "c"))
(define carol
  (c-vat
   (lambda ()
     (spawn ^carol))))
(define carol-sref
  (c-vat
   (lambda ()
     ($ c-mycapn 'register carol 'fake))))

;; Spawn alice on a with a reference to carol and bob
(define-values (a-vat a-netlayer a-mycapn)
  (make-new-machine "a"))
(define alice
  (a-vat
   (lambda ()
     ;; Bootstrap bob and carol with sturdyrefs
     (define bob-vow ($ a-mycapn 'enliven bob-sref))
     (define carol-vow ($ a-mycapn 'enliven carol-sref))
     (spawn ^alice bob-vow carol-vow))))

(let ((result
       (resolve-vow-and-return-result
        a-vat
        (lambda ()
          ($ alice)))))
  (test-equal
      "Check Alice on A can handoff Carol on C to Bob on B over CapTP"
    result
    #(ok "Hi Carol, I'm Bob")))

(test-end "test-captp")
