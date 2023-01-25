;;; Copyright 2019-2023 Christine Lemmer-Webber
;;; Copyright 2023 David Thompson
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(define-module (tests test-core)
  #:use-module (goblins core)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-11))

(test-begin "test-goblins-core")

;;; actormap low-level behavior tests
;;; =================================

(define-syntax-rule (snarf id ...)
  (begin
    (define id
      (@@ (goblins core) id))
    ...))

(snarf make-local-object-refr
       whactormap-set!
       whactormap-ref
       transactormap-set!
       transactormap-ref
       transactormap-merged?)

(define-syntax-rule (quietly body ...)
  (parameterize ((current-output-port (%make-void-port "w"))
                 (current-error-port (%make-void-port "w")))
    body ...))

;; set up actormap base with beeper and booper
(define actormap-base (make-whactormap))
(define beeper-refr (make-local-object-refr 'beeper #f))
(define (beeper-proc . args)
  'beep)
(whactormap-set! actormap-base beeper-refr beeper-proc)
(define booper-refr (make-local-object-refr 'booper #f))
(define (booper-proc . args)
  'boop)
(whactormap-set! actormap-base booper-refr booper-proc)
(define blepper-refr (make-local-object-refr 'blepper #f))
(define (blepper-proc . args)
  'blep)
(whactormap-set! actormap-base blepper-refr blepper-proc)

(define tam1
  (make-transactormap actormap-base))
(define bipper-refr (make-local-object-refr 'bipper #f))
(define (bipper-proc . args)
  'bippity)
(transactormap-set! tam1 bipper-refr bipper-proc)
(define (booper-proc2 . args)
  'boop2)
(transactormap-set! tam1 booper-refr booper-proc2)
(define (blepper-proc2 . args)
  'blep2)
(transactormap-set! tam1 blepper-refr blepper-proc2)
(test-eq bipper-proc
  (transactormap-ref tam1 bipper-refr))
(test-eq beeper-proc
  (transactormap-ref tam1 beeper-refr))
(test-eq booper-proc2
  (transactormap-ref tam1 booper-refr))
(test-eq blepper-proc2
  (transactormap-ref tam1 blepper-refr))
(test-eq booper-proc
  (whactormap-ref actormap-base booper-refr))
(test-assert (not (transactormap-merged? tam1)))


;;; actormap interface tests
;;; ========================

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

;; Let's noncommittally spawn our friend here...
(define-values (greety greety-tm)
  (actormap-spawn am ^greeter "Greety"))
;; We should be able to use actormap-peek on the transactormap...
(test-equal (actormap-peek greety-tm greety "Marge")
  "Hello Marge, my name is Greety!")
;; But we shouldn't be able to act on greety against the uncommitted
;; actormap, because nothing happened there...
(test-error #t (actormap-peek am greety "Marge"))
;; But now let's commmit it...
(transactormap-merge! greety-tm)
;; And now we should be able to.
(test-equal (actormap-peek am greety "Marge")
  "Hello Marge, my name is Greety!")

;; Test that peek and poke work right
(define a-ctr (actormap-spawn! am ^counter))
(test-equal (actormap-peek am a-ctr) 0)
(test-equal (actormap-peek am a-ctr) 0)
(test-equal (actormap-poke! am a-ctr) 0)
(test-equal (actormap-poke! am a-ctr) 1)
(test-equal (actormap-peek am a-ctr) 2)
(test-equal (actormap-peek am a-ctr) 2)
(test-equal (actormap-poke! am a-ctr) 2)
(test-equal (actormap-peek am a-ctr) 3)

;; Copy of the cell code from cell.scm.  Simplifies some
;; tests.

;; Constructor for a cell.  Takes an optional initial value, defaults
;; to false.
(define* (^cell bcom #:optional val)
  (case-lambda
    ;; Called with no arguments; return the current value
    [() val]
    ;; Called with one argument, we become a version of ourselves
    ;; with this new value
    [(new-val)
     (bcom (^cell bcom new-val))]))

(define (^spawns-during-constructor bcom)
  (define a-cell
    (spawn ^cell 'foo))
  (lambda ()
    (list 'got ($ a-cell))))
(define sdc
  (actormap-spawn! am ^spawns-during-constructor))

(test-equal "Spawn when we actormap-spawn(!) (yo dawg)"
  (actormap-peek am sdc)
  '(got foo))

;; Make sure using <-np queues a message
(test-eqv "a single message gets queued"
  (let-values (((_returned tam msgs)
                (actormap-run*
                 am
                 (lambda ()
                   (<-np alice "Nobody")))))
    (length msgs))
  1)

;; ... or three
(test-eqv "multiple messages get queued"
  (let-values (((_returned tam msgs)
                (actormap-run*
                 am
                 (lambda ()
                   (<-np alice "Nobody")
                   (<-np alice "Was")
                   (<-np alice "Here")))))
    (length msgs))
  3)


;; "on" handler for success case
(let ((on-result #f))
  (actormap-churn-run
   am
   (lambda ()
     (on (<- alice "Bob")
         (lambda (heard)
           (set! on-result `(heard-back ,heard))))))
  (test-equal "`on' handler success case"
    on-result '(heard-back "Hello Bob, my name is Alice!")))

(define (^explodable _bcom)
  (lambda _
    (error "oh YIKES")))

(let ((on-result #f))
  (quietly
   (actormap-churn-run
    am
    (lambda ()
      (define exploder (spawn ^explodable))
      (on (<- exploder)
          (lambda (heard)
            (set! on-result `(heard-back ,heard)))
          #:catch
          (lambda (exn)
            (set! on-result `(error ,exn)))))))
  (test-equal "`on' handler failure case"
    (match on-result
      [('error _err) #t]
      [_ #f])
    #t))

(define (^car-factory bcom company-name)
  (define (^car bcom model color)
    (lambda ()
      (format #f "*Vroom vroom!*  You drive your ~a ~a ~a!"
              color company-name model)))
  (define (make-car model color)
    (spawn ^car model color))
  make-car)

(let ((on-result #f))
  (actormap-churn-run!
   am
   (lambda ()
     (define fork-motors
       (spawn ^car-factory "Fork"))
     (define car-vow
       (<- fork-motors "Explorist" "blue"))
     (define drive-noise-vow
       (<- car-vow))
     (on drive-noise-vow
         (lambda (heard)
           (set! on-result `(heard-back ,heard))))))
  (test-equal "Pipelining works in simplest near case"
    on-result '(heard-back "*Vroom vroom!*  You drive your blue Fork Explorist!")))

(define (^lessgood-car-factory bcom company-name)
  (define (^car bcom model color)
    (lambda ()
      (format #f "*Vroom vroom!*  You drive your ~a ~a ~a!"
              color company-name model)))
  (define (make-car model color)
    (error "Your car exploded on the factory floor!  Ooops!")
    (spawn ^car model color))
  make-car)

(let ((on-result #f))
  (quietly
   (actormap-churn-run!
    am
    (lambda ()
      (define fork-motors
        (spawn ^lessgood-car-factory "Forked"))
      (define car-vow
        (<- fork-motors "Exploder" "red"))
      (define drive-noise-vow
        (<- car-vow))
      (on drive-noise-vow
          (lambda (heard)
            (set! on-result `(heard-back ,heard)))
          #:catch
          (lambda (err)
            (set! on-result `(err ,err)))))))
  (test-assert "Errors propagate through a promise pipeline"
    (match on-result
      (('err _err) #t)
      (_ #f))))

;; And here's the other variant of promise pipelining breakage
(let ([what-i-got #f])
  (quietly
   (actormap-churn-run!
    am (lambda ()
         (define (^broken-actor _bcom)
           (lambda _
             (error "I am error")))
         (define (^returns-actor _bcom return-me)
           (lambda ()
             return-me))
         (define broken-actor
           (spawn ^broken-actor))
         (define returns-broken-actor
           (spawn ^returns-actor broken-actor))
         (define broken-actor-vow
           (<- returns-broken-actor))
         (on (<- broken-actor-vow)
             (lambda (v)
               (set! what-i-got `(yeah ,v)))
             #:catch
             (lambda (e)
               (set! what-i-got `(oh-no ,e)))))))
  (test-equal
   "Errors propagate through a promise pipeline, other version"
   (car what-i-got)
   'oh-no))

(test-assert "raised exceptions are actormap turn errors"
  ;; The only way this returns #t is if an actormap turn error is
  ;; handled.
  (with-exception-handler (const #t)
    (lambda ()
      (quietly (actormap-churn-run! am (lambda () (+ 1 "two"))))
      ;; If the actormap churn didn't throw an error and the test
      ;; made it here, it would fail.
      #f)
    #:unwind? #t
    #:unwind-for-type &actormap-turn-error))

(define bob (actormap-spawn! am ^cell "Hi, I'm bob!"))
(define bob-promise-and-resolver
  (actormap-run! am spawn-promise-cons))

(define bob-vow (car bob-promise-and-resolver))
(define bob-resolver (cdr bob-promise-and-resolver))

(snarf mactor:local-link?)

(actormap-poke! am bob-resolver 'fulfill bob)
(test-assert
 "Promise resolves to local-link"
 (mactor:local-link? (whactormap-ref am bob-vow)))
(test-equal
 "Resolved local-link acts as what it resolves to"
 (actormap-peek am bob-vow)
 "Hi, I'm bob!")
(actormap-poke! am bob-vow "Hi, I'm bobby!")
(test-equal
 "Resolved local-link can change original"
 (actormap-peek am bob)
 "Hi, I'm bobby!")

(define on-resolved-bob-arg #f)
(actormap-churn-run!
 am (lambda ()
      (on bob-vow
          (lambda (v)
            (set! on-resolved-bob-arg v)))))
(test-equal
 "Using `on' against a resolved refr returns that refr"
 on-resolved-bob-arg bob)

(snarf near-settled-promise-value)

(test-eq
 "near-settled-promise-value can extract local-refr value"
 (actormap-run
  am (lambda ()
       (near-settled-promise-value bob-vow)))
 bob)

(define encase-vow-and-resolver
  (actormap-run! am spawn-promise-cons))

(define encase-me-vow
  (car encase-vow-and-resolver))
(define encase-me-resolver
  (cdr encase-vow-and-resolver))
(actormap-poke! am encase-me-resolver 'fulfill 'encase-me)
(test-eq
 "extracting encased value via actormap-peek"
 (actormap-peek am encase-me-vow)
 'encase-me)
(test-eq
 "extracting encased value via $"
 (actormap-run am (lambda () ($ encase-me-vow)))
 'encase-me)

(define on-resolved-encased-arg #f)
(actormap-churn-run!
 am (lambda ()
      (on encase-me-vow
          (lambda (v)
            (set! on-resolved-encased-arg v)))))
(test-equal
 "Using `on' against a resolved refr returns that refr"
 on-resolved-encased-arg 'encase-me)


;; Tests for propagation of resolutions
(define (try-out-on actormap . resolve-args)
  ;; Run on against a promise, store the results
  ;; in the following cells
  (define resolved-cells
    (actormap-churn-run!
     actormap
     (lambda ()
       (define fulfilled-cell (spawn ^cell #f))
       (define broken-cell (spawn ^cell #f))
       (define finally-cell (spawn ^cell #f))
       (define-values (a-vow a-resolver)
         (spawn-promise-values))
       (on a-vow
           (lambda args
             ($ fulfilled-cell args))
           #:catch
           (lambda args
             ($ broken-cell args))
           #:finally
           (lambda ()
             ($ finally-cell #t)))
       (apply <-np a-resolver resolve-args)
       (list fulfilled-cell broken-cell finally-cell))))
  (map (lambda (cell)
         (actormap-peek actormap cell))
       resolved-cells))

(test-equal
 "Fulfilling a promise with on"
 (try-out-on am 'fulfill 'how-fulfilling)
 '((how-fulfilling) #f #t))

(test-equal
 "Breaking a promise with on"
 (try-out-on am 'break 'i-am-broken)
 '(#f (i-am-broken) #t))

(define (spawn-const val)
  (spawn (lambda _ (lambda _ val))))
(define (spawn-proc proc)
  (spawn (lambda _ proc)))

(let ([what-i-got #f])
  (actormap-churn-run!
   am (lambda ()
        (on (<- (spawn-const 'i-am-foo))
            (lambda (v)
              (set! what-i-got `(yeah ,v)))
            #:catch
            (lambda (e)
              (set! what-i-got `(oh-no ,e))))))
  (test-equal
   "<- returns a listen'able promise"
   what-i-got
   '(yeah i-am-foo)))

(let ([what-i-got #f])
  (quietly
   (actormap-churn-run!
    am (lambda ()
         (on (<- (spawn-proc
                  (lambda _
                    (error "I am error"))))
             (lambda (v)
               (set! what-i-got `(yeah ,v)))
             #:catch
             (lambda (e)
               (set! what-i-got `(oh-no ,e)))))))
  (test-equal
   "<- promise breaks as expected"
   (car what-i-got)
   'oh-no))

(let ([what-i-got #f])
  (actormap-churn-run!
   am (lambda ()
        (define foo (spawn-const 'i-am-foo))
        (on (<- (<- (spawn-proc (lambda _ foo))))
            (lambda (v)
              (set! what-i-got `(yeah ,v)))
            #:catch
            (lambda (e)
              (set! what-i-got `(oh-no ,e))))))
  (test-equal
   "basic promise pipelining"
   what-i-got
   '(yeah i-am-foo)))

(let ([what-i-got #f])
  (quietly
   (actormap-churn-run!
    am (lambda ()
         (define fatal-foo
           (spawn-proc
            (lambda _
              (error "I am error"))))
         (on (<- (<- (spawn-proc (lambda _ fatal-foo))))
             (lambda (v)
               (set! what-i-got `(yeah ,v)))
             #:catch
             (lambda (e)
               (set! what-i-got `(oh-no ,e)))))))
  (test-equal
   "basic promise contagion"
   (car what-i-got)
   'oh-no))

(test-equal
 "Passing #:promise? to `on` returns a promise that is resolved"
 (actormap-peek
  am
  (actormap-churn-run!
   am (lambda ()
        (define doubler (spawn (lambda (bcom)
                                 (lambda (x)
                                   (* x 2)))))
        (define the-on-promise
          (on (<- doubler 3)
              (lambda (x)
                (format #f "got: ~a" x))
              #:catch
              (lambda (e)
                "uhoh")
              #:promise? #t))
        the-on-promise)))
 "got: 6")

(let ([what-i-got #f]
      [finally-also-ran? #f])
  (actormap-churn-run!
   am
   (lambda ()
     (on 42
         (lambda (val)
           (set! what-i-got (format #f "got: ~a" val)))
         #:finally
         (lambda ()
           (set! finally-also-ran? #t)))))
  (test-equal
   "A non-promise value passed to `on` merely resolves to that value"
   what-i-got
   "got: 42")
  (test-assert
   "#:finally also runs in case of non-promise value passed to `on`"
   finally-also-ran?))

;; verify listen-to works
(define (try-out-listen-to . resolve-args)
  (let ([resolved-val #f]
        [resolved-err #f])
    (actormap-churn-run!
     am
     (lambda ()
       (define promise-and-resolver (actormap-run! am spawn-promise-cons))
       (define some-promise (car promise-and-resolver))
       (define some-resolver (cdr promise-and-resolver))
       (listen-to some-promise
                  (spawn
                   (lambda (bcom)
                     (match-lambda*
                       [('fulfill val)
                        (set! resolved-val `(fulfilled ,val))]
                       [('break err)
                        (set! resolved-err `(broken ,err))]))))
       (apply $ some-resolver resolve-args)))
    (list resolved-val resolved-err)))
(test-equal
 "listen-to works with a fulfilled promise"
 (try-out-listen-to 'fulfill 'yay)
 '((fulfilled yay) #f))
(test-equal
 "listen-to works with a broken promise"
 (try-out-listen-to 'break 'oh-no)
 '(#f (broken oh-no)))


(define (try-promise-to-promise . resolve-args)
  (let ([result #f]
        [finally-ran? #f])
    (actormap-churn-run!
     am
     (lambda ()
       (define-values (listen-to-promise listen-to-resolver)
         (spawn-promise-values))
       (define-values (middle-promise middle-resolver)
         (spawn-promise-values))
       (define-values (gets-the-answer-promise gets-the-answer-resolver)
         (spawn-promise-values))
       (on listen-to-promise
           (lambda (val)
             (set! result `(got-val ,val)))
           #:catch
           (lambda (err)
             (set! result `(got-err ,err)))
           #:finally
           (lambda ()
             (set! finally-ran? #t)))
       (<-np listen-to-resolver 'fulfill middle-promise)
       (<-np middle-resolver 'fulfill gets-the-answer-promise)
       (apply <-np gets-the-answer-resolver resolve-args)))
    (list result finally-ran?)))

(test-equal
 "Promise fulfilled to promise itself gets fulfillment"
 (try-promise-to-promise 'fulfill 'yay)
 '((got-val yay) #t))

(test-equal
 "Promise fulfilled to promise has broken promise contagion"
 (try-promise-to-promise 'break 'yikes)
 '((got-err yikes) #t))

(test-end "test-goblins-core")
