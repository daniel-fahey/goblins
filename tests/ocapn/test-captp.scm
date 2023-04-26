(define-module (tests ocapn test-captp)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn netlayer fake)
  #:use-module (tests utils)
  #:use-module (fibers channels)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(test-begin "test-captp")

(define (make-new-machine name)
  "Create a new vat, spawns a fake netlayer & mycapn for given `name'"
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

(define test-vat (spawn-vat #:name "test"))
(define test-network
  (with-vat test-vat
   (spawn ^fake-network)))


;; Spawn different machines.
(define-values (a-vat a-netlayer a-mycapn)
  (make-new-machine "a"))
(define-values (b-vat b-netlayer b-mycapn)
  (make-new-machine "b"))
(define-values (c-vat c-netlayer c-mycapn)
  (make-new-machine "c"))

(define (^greeter _bcom our-name)
  (lambda (their-name)
    (format #f "Hello ~a, my name is ~a" their-name our-name)))

(define bob-sref
  (with-vat b-vat
    (define bob (spawn ^greeter "Bob"))
    ($ b-mycapn 'register bob 'fake)))

(let ((result
       (resolve-vow-and-return-result
        a-vat
        (lambda ()
          (on (<- a-mycapn 'enliven bob-sref)
              (lambda (bob)
                (<- bob "Alyssa"))
              #:promise? #t)))))
  (test-equal "Non-pipelined send to bob over CapTP"
    result
    #(ok "Hello Alyssa, my name is Bob")))


;; Testing promise error propagation over CapTP.
(define (^broken-greeter _bcom our-name)
  (lambda (their-name)
    (error 'oh-no-i-broke "My name:" our-name)
    (format #f "Hello ~a, my name is ~a" their-name our-name)))

(define broken-bob-sref
  (with-vat b-vat
    (define broken-bob (spawn ^broken-greeter "Broken Bob"))
    ($ b-mycapn 'register broken-bob 'fake)))

(let ((result
       (resolve-vow-and-return-result
        a-vat
        (lambda ()
          (<- (<- a-mycapn 'enliven broken-bob-sref) "Alyssa")))))
  (test-assert "Promise pipelined breakage propagates across CapTP"
    (match result
      [#('err problem) #t]
      [_ #f])))

;; TODO: This test, particularly the new vat that is spawned for it,
;; is causing intermittent test failures, so we've disabled it until
;; we can determine the root cause and a fix.  See
;; https://gitlab.com/spritely/guile-goblins/-/issues/69 for more
;; information.

;; Testing multiple objects on A, communicating with B
;; (define a1-vat (spawn-vat #:name "a1"))
;; (define introducer-alice
;;   (with-vat a-vat
;;     (define (^introducer-alice _bcom)
;;       (lambda (intro-bob intro-carol)
;;         (<- intro-bob 'meet intro-carol)))
;;     (spawn ^introducer-alice)))

;; (define meeter-bob-sref
;;   (with-vat b-vat
;;     (define (^meeter-bob _bcom)
;;       (methods
;;        [(meet new-friend)
;;         (<- new-friend 'hi-new-friend)]))
;;     (define meeter-bob (spawn ^meeter-bob))
;;     ($ b-mycapn 'register meeter-bob 'fake)))

;; (define (^chatty _bcom our-name)
;;   (methods
;;    [(hi-new-friend)
;;     (list 'hello-back-from our-name)]))

;; (define chatty-carol
;;   (with-vat a1-vat
;;     (spawn ^chatty 'carol)))

;; (let ((result
;;        (resolve-vow-and-return-result
;;         a-vat
;;         (lambda ()
;;           (on (<- a-mycapn 'enliven meeter-bob-sref)
;;               (lambda (meeter-bob)
;;                 (<- introducer-alice meeter-bob chatty-carol))
;;               #:promise? #t)))))
;;   (test-equal "A and C on one machine, B on another with introductions"
;;     result
;;     #(ok (hello-back-from carol))))

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

;; Spawn bob on b
(define bob
  (with-vat b-vat
    (spawn ^bob)))
(define bob-sref
  (with-vat b-vat
   ($ b-mycapn 'register bob 'fake)))

;; Spawn carol on c
(define carol
  (with-vat c-vat
   (spawn ^carol)))
(define carol-sref
  (with-vat c-vat
   ($ c-mycapn 'register carol 'fake)))

;; Spawn alice on a with a reference to carol and bob
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
      "Alice on A, Bob on B introduces Carol on C, via handoffs"
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

;; Test promise shortening
(define sword
  (with-vat a-vat
    (spawn (lambda (_bcom) (lambda () 'sword)))))
(define sword-sref
  (with-vat a-vat
    ($ a-mycapn 'register sword 'fake)))
(define intermediate-land-sref
  (with-vat b-vat
    (define (^intermediate-land _bcom)
      (lambda (farthest-land)
        (<- farthest-land)))
    (define intermediate-land (spawn ^intermediate-land))
    ($ b-mycapn 'register intermediate-land 'fake)))
(define farthest-land-sref
  (with-vat c-vat
    (define (^farthest-land _bcom lost-sword)
      (lambda ()
        lost-sword))
    (define farthest-land
      (spawn ^farthest-land (<- c-mycapn 'enliven sword-sref)))
    ($ c-mycapn 'register farthest-land 'fake)))
(define adventurer
  (with-vat a-vat
    (define (^adventurer _bcom sword)
      (lambda (intermediate-land farthest-land)
        (on (<- intermediate-land farthest-land)
            (lambda (returned-sword)
              `(is-the-same? ,(eq? returned-sword sword)))
            #:catch
            (lambda (err)
              `(error ,err))
            #:promise? #t)))
    (spawn ^adventurer sword)))

(let ((result
       (resolve-vow-and-return-result
        a-vat
        (lambda ()
          (<- adventurer
              (<- a-mycapn 'enliven intermediate-land-sref)
              (<- a-mycapn 'enliven farthest-land-sref))))))
  (test-equal "Shortening promises across CapTP"
    result
    #(ok (is-the-same? #t))))



(test-end "test-captp")
