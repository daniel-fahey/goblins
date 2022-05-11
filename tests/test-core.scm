;;; Copyright 2019-2021 Christine Lemmer-Webber
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
  (actormap-churn-run
   am
   (lambda ()
     (define exploder (spawn ^explodable))
     (on (<- exploder)
         (lambda (heard)
           (set! on-result `(heard-back ,heard)))
         #:catch
         (lambda (exn)
           (set! on-result `(error ,exn))))))
  (test-equal "`on' handler success case"
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
           (set! on-result `(err ,err))))))
  (test-assert "Pipelining errors are contagious"
    (match on-result
      (('err _err) #t)
      (_ #f))))

;; And here's the other variant of promise pipelining breakage
(let ([what-i-got #f])
  (actormap-churn-run!
   am (lambda ()
        (define fatal-foo
          (spawn
           (lambda _
             (lambda _
               (error "I am error")))))
        (on (<- (<- (spawn (lambda _ (lambda _ fatal-foo)))))
            (lambda (v)
              (set! what-i-got `(yeah ,v)))
            #:catch
            (lambda (e)
              (set! what-i-got `(oh-no ,e))))))
  (test-equal
   "Promise pipelining broken promise contagion, other version"
   (car what-i-got)
   'oh-no))

(test-end "test-goblins-core")
