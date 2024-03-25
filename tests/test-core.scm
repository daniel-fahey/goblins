;;; Copyright 2019-2023 Christine Lemmer-Webber
;;; Copyright 2023 David Thompson
;;; Copyright 2024 Jessica Tallon
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
  #:use-module (goblins core-types)
  #:use-module (goblins abstract-types)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
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

(test-equal "Test an actor which calls another actor"
  "I heard back: Hello Greg, my name is Alice!"
  (actormap-peek am greg alice))

;; Actor updates: update and return value separately
(define* (^cell bcom #:optional [val #f])
  (case-lambda
    [() val]
    [(new-val) (bcom (^cell bcom new-val))]))

(actormap-run!
 am
 (lambda ()
   (define cell (spawn ^cell))
   (test-equal ($ cell) #f)                  ; initial val
   (test-equal ($ cell 'foo) *unspecified*)  ; update (no return value)
   (test-equal ($ cell) 'foo)))              ; new val

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

(test-equal "Test actormap-peek works on transactormaps"
  "Hello Marge, my name is Greety!"
  (actormap-peek greety-tm greety "Marge"))
;; But we shouldn't be able to act on greety against the uncommitted
;; actormap, because nothing happened there...
(test-error (actormap-peek am greety "Marge"))
;; But now let's commmit it...
(transactormap-merge! greety-tm)
;; And now we should be able to.
(test-equal "Check actor can be used (peek'ed) after committed to transactormap"
  "Hello Marge, my name is Greety!"
  (actormap-peek am greety "Marge"))

;; Test that peek and poke work right
(define a-ctr (actormap-spawn! am ^counter))
(test-equal 0 (actormap-peek am a-ctr))
(test-equal 0 (actormap-peek am a-ctr))
(test-equal 0 (actormap-poke! am a-ctr))
(test-equal 1 (actormap-poke! am a-ctr))
(test-equal 2 (actormap-peek am a-ctr))
(test-equal 2 (actormap-peek am a-ctr))
(test-equal 2 (actormap-poke! am a-ctr))
(test-equal 3 (actormap-peek am a-ctr))

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
  '(got foo)
  (actormap-peek am sdc))

;; Make sure using <-np queues a message
(test-eqv "a single message gets queued"
  1
  (let-values (((_returned tam msgs)
                (actormap-run*
                 am
                 (lambda ()
                   (<-np alice "Nobody")))))
    (length msgs)))

;; ... or three
(test-eqv "multiple messages get queued"
  3
  (let-values (((_returned tam msgs)
                (actormap-run*
                 am
                 (lambda ()
                   (<-np alice "Nobody")
                   (<-np alice "Was")
                   (<-np alice "Here")))))
    (length msgs)))


;; "on" handler for success case
(let ((on-result #f))
  (actormap-churn-run
   am
   (lambda ()
     (on (<- alice "Bob")
         (lambda (heard)
           (set! on-result `(heard-back ,heard))))))
  (test-equal "`on' handler success case"
    '(heard-back "Hello Bob, my name is Alice!")
    on-result))

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
  (test-assert "`on' handler failure case"
    (match on-result
      [('error _err) #t]
      [_ #f])))

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
    '(heard-back "*Vroom vroom!*  You drive your blue Fork Explorist!")
    on-result))

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
  (test-equal "Errors propagate through a promise pipeline, other version"
   'oh-no
   (car what-i-got)))

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
(test-equal "Resolved local-link acts as what it resolves to"
 "Hi, I'm bob!"
 (actormap-peek am bob-vow))
(actormap-poke! am bob-vow "Hi, I'm bobby!")
(test-equal "Resolved local-link can change original"
 "Hi, I'm bobby!"
 (actormap-peek am bob))

(define on-resolved-bob-arg #f)
(actormap-churn-run!
 am (lambda ()
      (on bob-vow
          (lambda (v)
            (set! on-resolved-bob-arg v)))))
(test-equal "Using `on' against a resolved refr returns that refr"
  bob
  on-resolved-bob-arg)

(snarf near-settled-promise-value)

(test-eq "near-settled-promise-value can extract local-refr value"
 bob
 (actormap-run
  am (lambda ()
       (near-settled-promise-value bob-vow))))

(define encase-vow-and-resolver
  (actormap-run! am spawn-promise-cons))

(define encase-me-vow
  (car encase-vow-and-resolver))
(define encase-me-resolver
  (cdr encase-vow-and-resolver))
(actormap-poke! am encase-me-resolver 'fulfill 'encase-me)
(test-eq "extracting encased value via actormap-peek"
 'encase-me
 (actormap-peek am encase-me-vow))
(test-eq "extracting encased value via $"
 'encase-me
 (actormap-run am (lambda () ($ encase-me-vow))))

(define on-resolved-encased-arg #f)
(actormap-churn-run!
 am (lambda ()
      (on encase-me-vow
          (lambda (v)
            (set! on-resolved-encased-arg v)))))
(test-equal "Using `on' against a resolved refr returns that refr"
  'encase-me
  on-resolved-encased-arg)


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

(test-equal "Fulfilling a promise with on"
 '((how-fulfilling) #f #t)
 (try-out-on am 'fulfill 'how-fulfilling))

(test-equal "Breaking a promise with on"
 '(#f (i-am-broken) #t)
 (try-out-on am 'break 'i-am-broken))

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
  (test-equal "<- returns a listen'able promise"
   '(yeah i-am-foo)
   what-i-got))

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
  (test-equal "<- promise breaks as expected"
   'oh-no
   (car what-i-got)))

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
  (test-equal "basic promise pipelining"
   '(yeah i-am-foo)
   what-i-got))

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
  (test-equal "basic promise contagion"
   'oh-no
   (car what-i-got)))

(test-equal "Passing #:promise? to `on` returns a promise that is resolved"
 "got: 6"
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
        the-on-promise))))

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
   "got: 42"
   what-i-got)
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
(test-equal "listen-to works with a fulfilled promise"
 '((fulfilled yay) #f)
 (try-out-listen-to 'fulfill 'yay))
(test-equal "listen-to works with a broken promise"
 '(#f (broken oh-no))
 (try-out-listen-to 'break 'oh-no))


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

(test-equal "Promise fulfilled to promise itself gets fulfillment"
 '((got-val yay) #t)
 (try-promise-to-promise 'fulfill 'yay))

(test-equal "Promise fulfilled to promise has broken promise contagion"
 '((got-err yikes) #t)
 (try-promise-to-promise 'break 'yikes))

;; object persistence tests
(define* (^incrementer bcom #:optional [value 0])
  (define (main-beh)
    (bcom (^incrementer bcom (+ 1 value)) (+ 1 value)))
  (define (self-portrait)
    (list value))
  (portraitize main-beh self-portrait))

(define ^persistent-greeter
  (make-redefinable-object
   (lambda* (bcom our-name #:optional [init-number-of-times #f])
     (define number-of-times
       (or init-number-of-times (spawn ^incrementer)))
     (define (main-beh your-name)
       (bcom (^persistent-greeter bcom our-name number-of-times)
             (format #f "Hello ~a, my name is ~a (called ~a)."
                     your-name our-name ($ number-of-times))))
     (define (self-portrait)
       (list our-name number-of-times))
     (portraitize main-beh self-portrait))))

(define (restored-greeter-rehydrate version our-name number-of-times)
  (spawn ^persistent-greeter
         (format #f "*restored ~a*" our-name)
         number-of-times))

(define first-actormap
  (make-actormap))

(define astrid-greeter
  (actormap-run!
   first-actormap
   (lambda ()
     (define astrid
       (spawn ^persistent-greeter "Astrid"))
     ($ astrid "Lars")
     ($ astrid "Johan")
     astrid)))

(define incrementer-env
  (make-persistence-env
   (list (list '((tests test-core) ^incrementer) ^incrementer))))

(define greeter-env
  (make-persistence-env
   (list (list '((tests test-core) ^persistent-greeter) ^persistent-greeter restored-greeter-rehydrate))
   #:extends incrementer-env))

(define-values (astrid-greeter-portrait astrid-greeter-roots)
  (actormap-take-portrait first-actormap greeter-env astrid-greeter))

(define second-actormap
  (make-actormap))

(define restored-astrid-greeter
  (actormap-restore! second-actormap greeter-env astrid-greeter-portrait astrid-greeter-roots))

(test-equal "Restored greeter reports correct number of times called"
  (actormap-peek second-actormap restored-astrid-greeter "Ludvig")
  "Hello Ludvig, my name is *restored Astrid* (called 3).")


(set-redefinable-object-constructor!
 ^persistent-greeter
 (lambda* (bcom our-name #:optional init-number-of-times)
   (define number-of-times
     (or init-number-of-times (spawn ^incrementer)))
   (define (main-beh your-name)
     (format #f "Salutations ~a, I am called ~a, delighted to make your acquaintance! (called: ~a)"
             your-name our-name ($ number-of-times)))
   (define (self-portrait)
     (list our-name number-of-times))
   (portraitize main-beh self-portrait)))

(actormap-replace-behavior! first-actormap greeter-env)

(test-equal "Actors are updated when the behavior is replaced by new behavior"
  (actormap-peek first-actormap astrid-greeter "Ludvig")
  "Salutations Ludvig, I am called *restored Astrid*, delighted to make your acquaintance! (called: 3)")

;; Test that when we restore what we get back is eq?
(define (^ro-cell _bcom value)
  (define (main-beh)
    value)
  (define (self-portrait)
    (list value))
  (portraitize main-beh self-portrait))
(define ro-cell-env
  (make-persistence-env
   `((((tests test-core) ^ro-cell) ,^ro-cell))
     #:extends incrementer-env))
(define incrementer
  (actormap-spawn! first-actormap ^incrementer))
(define ro-cell
  (actormap-spawn! first-actormap ^ro-cell incrementer))

(define-values (portraits root-slots)
  (actormap-take-portrait first-actormap ro-cell-env incrementer ro-cell))
(define-values (incrementer* ro-cell*)
  (actormap-restore! second-actormap ro-cell-env portraits root-slots))

(test-eq "Restored actor is eq?"
  incrementer*
  (actormap-peek second-actormap ro-cell*))

(define (^bar _bcom value)
  (define (main-beh another-value)
    (list value another-value))
  (define (self-portrait)
    (list value))
  (portraitize main-beh self-portrait))

(define (^foo _bcom bar value)
  (define barred-value (<- bar value))
  (define (main-beh)
    barred-value)
  (define (self-portrait)
    (list bar value))
  (portraitize main-beh self-portrait))

(define env
  (make-persistence-env
   `((((tests test-core) ^bar) ,^bar)
     (((tests test-core) ^foo) ,^foo))))

(test-equal "Check restored actors can send messages upon construction"
  '(start-bar start-foo)
  (let*-values (((am1) (make-actormap))
		((am2) (make-actormap))
		((bar1) (actormap-spawn! am1 ^bar 'start-bar))
		((foo1) (actormap-spawn! am1 ^foo bar1 'start-foo))
		((portraits roots) (actormap-take-portrait am1 env foo1))
		((foo2) (actormap-restore! am2 env portraits roots)))
    (actormap-peek
     am2
     (actormap-churn-run!
      am2
      (lambda ()
	($ foo2))))))

;; Test spawning another actor in the restore behavior
(define ^second
  (make-redefinable-object
   (lambda* (bcom #:optional stored-val)
     (define* (main-beh #:optional new-val)
       (if new-val
	   (bcom (^second bcom new-val) 'second)
	   (list 'second stored-val)))
     (define (self-portrait)
       (list stored-val))
     (portraitize main-beh self-portrait))))
(define ^first
  (make-redefinable-object
   (lambda (_bcom)
     (define (main-beh)
       'first)
     (define (self-portrait)
       (list))
     (portraitize main-beh self-portrait))))
(define first (actormap-spawn! first-actormap ^first))
(define env
  (make-persistence-env
   `((((tests test-core) ^first) ,^first)
     (((tests test-core) ^second) ,^second))))
;; Redefine it so that we spawn another object while being constructed!
(set-redefinable-object-constructor!
 ^first
 (lambda (_bcom)
   (define second (spawn ^second #f))
   (<-np second 'pineapple)
   (lambda ()
     ($ second))))

(actormap-replace-behavior! first-actormap env)
(test-equal "Redefinable objects can spawn other objects on construction"
  '(second pineapple)
  (actormap-peek first-actormap first))

;; Test all supported types can be serialized correctly
;; NOTE: ghash and gset tested by common's tests
(define* (^type-serializer bcom supplied-refr
			   #:optional
			   got-number got-symbol got-list
			   got-keyword got-zilch got-tagged
			   got-string got-bv got-bool
			   got-unspecified got-vector
			   got-near-refr
			   got-promise-to-refr
			   got-promise-to-value)
  ;; Make the promises
  (define-values (refr-vow refr-resolver)
    (spawn-promise-values))
  ($ refr-resolver 'fulfill supplied-refr)
  (define-values (encased-vow encased-resolver)
    (spawn-promise-values))
  ($ encased-resolver 'fulfill 75)

  ;; Make the other values
  (define number 7.2)
  (define symbol 'hello)
  (define my-vector #(1 2 3 zilch))
  (define my-list (list 1 2 3 zilch))
  (define bv (make-bytevector 10 10))
  (define string "tada 🪄")
  (define keyword #:i-am-a-keyword)
  (define tagged (make-tagged 'foo supplied-refr))
  (define bool #t)

  (define (main-beh restored-refr)
    (and (eq? got-number number)
	 (eq? got-symbol symbol)
	 (equal? got-list my-list)
	 (eq? got-keyword keyword)
	 (eq? got-zilch zilch)
	 (equal? got-tagged (make-tagged 'foo restored-refr))
	 (string=? got-string string)
	 (equal? got-bv bv)
	 (eq? got-bool bool)
	 (unspecified? got-unspecified)
	 (equal? my-vector got-vector)
	 (eq? restored-refr got-near-refr)
	 (live-refr? got-promise-to-refr)
	 (eq? ($ encased-vow) ($ got-promise-to-value))))

  (define (self-portrait)
    (list #f
	  number symbol my-list keyword zilch
	  tagged string bv bool *unspecified*
	  my-vector supplied-refr refr-vow encased-vow))
  (portraitize main-beh self-portrait))
(define env
  (make-persistence-env
   `((((tests test-core) ^type-serializer) ,^type-serializer)
     (((tests test-core) ^bar) ,^bar))))

(define bar (actormap-spawn! first-actormap ^bar 'foo))
(define types (actormap-spawn! first-actormap ^type-serializer bar))
(define-values (portraits slots)
  (actormap-take-portrait first-actormap env types bar))
(define-values (types* bar*)
  (actormap-restore first-actormap env portraits slots))
(test-assert "All serializable types can be serialized by aurie"
  (actormap-peek first-actormap types* bar*))

(test-end "test-goblins-core")
