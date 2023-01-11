(use-modules (goblins)
             (goblins vat)
             (goblins actor-lib joiners)
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
  (with-vat test-vat
   (spawn ^fake-network)))

(define (make-new-machine name)
  (define machine-vat (spawn-vat #:name name))
  (define new-conn-ch (make-channel))
  (with-vat test-vat
    ($ test-network 'register name new-conn-ch))
  (define location (make-ocapn-machine 'fake name #f))
  (define netlayer
    (with-vat machine-vat
     (spawn ^fake-netlayer name test-network new-conn-ch)))
  (define mycapn
    (with-vat machine-vat
     (spawn-mycapn netlayer)))
  (values machine-vat netlayer mycapn))

;; Spawn bob on b
(define-values (b-vat b-netlayer b-mycapn)
  (make-new-machine "b"))
(define bob
  (with-vat b-vat
    (spawn ^bob)))
(define bob-sref
  (with-vat b-vat
   ($ b-mycapn 'register bob 'fake)))

;; Spawn carol on c
(define-values (c-vat c-netlayer c-mycapn)
  (make-new-machine "c"))
(define carol
  (with-vat c-vat
   (spawn ^carol)))
(define carol-sref
  (with-vat c-vat
   ($ c-mycapn 'register carol 'fake)))

;; Spawn alice on a with a reference to carol and bob
(define-values (a-vat a-netlayer a-mycapn)
  (make-new-machine "a"))
(define alice
  (with-vat a-vat
    ;; Bootstrap bob and carol with sturdyrefs
    (define bob-vow ($ a-mycapn 'enliven bob-sref))
    (define carol-vow ($ a-mycapn 'enliven carol-sref))
    ;; The vows need to be resolved for this to perform a handoff
    (on (all-of bob-vow carol-vow)
        (lambda (bob-carol-pair)
          (spawn ^alice (car bob-carol-pair) (car (cdr bob-carol-pair))))
        #:promise? #t)))

(let ((result
       (resolve-vow-and-return-result
        a-vat
        (lambda ()
          (<- alice)))))
  (test-equal
      "Check Alice on A can handoff Carol on C to Bob on B over CapTP"
    result
    #(ok "Hi Carol, I'm Bob")))

(define (^kw-car-factory _bcom brand)
  (lambda* (model #:key (color #f) (noise #f))
    (define (^car _bcom)
      (lambda ()
        (format #f "a ~a ~a ~a goes ~a!" color brand model noise)))
    (spawn ^car)))

(let* ((fork-factory (with-vat a-vat
                       (spawn ^kw-car-factory "fork")))
       (fork-factory-sref (with-vat a-vat
                            ($ a-mycapn 'register fork-factory 'fake)))
       (fork-factory-vow (with-vat b-vat
                           (<- b-mycapn 'enliven fork-factory-sref)))
       (red-explorist-vow (with-vat b-vat
                            (<- fork-factory-vow "explorist"
                                #:color "red" #:noise "vrooom")))
       (result
        (resolve-vow-and-return-result
         b-vat
         (lambda ()
           (<- red-explorist-vow)))))
  (test-equal "Sending keyword arguments over CapTP"
    result
    #(ok "a red fork explorist goes vrooom!")))

(test-end "test-captp")
