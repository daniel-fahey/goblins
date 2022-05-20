;;; Copyright 2019-2022 Christine Lemmer-Webber
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

(define-module (goblins-perf-test)
  #:use-module (goblins core)
  #:use-module (ice-9 match))

(define (repeat n thunk)
  (let lp ([i n])
    (unless (zero? i)
      (thunk)
      (lp (1- i)))))

(define* (do-actors-gc #:optional [actormap (make-actormap)] [n 10000])
  (define (^simple-actor bcom)
    (lambda ()
      'hello))
  (repeat
   n
   (lambda ()
     (define friend (actormap-spawn! actormap ^simple-actor))
     (actormap-poke! actormap friend))))

(define* (do-self-referential-actors-gc #:optional [actormap (make-actormap)])
  (define (spawn-simple-actor)
    (define (^simple-actor bcom)
      (match-lambda
       ['self self]
       ['oop 'boop]))
    (define self (spawn ^simple-actor))
    self)
  (repeat 1000000
          (lambda ()
            (actormap-run!
             actormap
             (lambda ()
               (define friend (spawn-simple-actor))
               ($ friend 'self)
               'no-op)))))


;;; In Racket, 2019-10-29
;; perf-tests.rkt> (call-a-lot)
;; cpu time: 991 real time: 990 gc time: 5

;;;; In Racket, 2022-05-11:
;; perf-tests.rkt> (call-a-lot)
;; cpu time: 2123 real time: 2123 gc time: 19
;; perf-tests.rkt> (call-a-lot)
;; cpu time: 650 real time: 650 gc time: 4

;;;; In Guile, 2022-05-11:
;; scheme@(goblins-perf-test)> ,time (call-a-lot)
;; ;; 0.333687s real time, 1.107596s run time.  0.868718s spent in GC.

;; So Guile is faster in execution on this currently, but slower in GC.
;; We can live with that.
(define* (call-a-lot #:optional [actormap (make-actormap)])
  (define (^simple-actor bcom)
    (lambda ()
      'hello))
  (actormap-run!
   actormap
   (lambda ()
     (define friend
       (spawn ^simple-actor))
     (let lp ([i 1000000])
       (unless (zero? i)
         ($ friend)
         (lp (1- i)))))))


;;; Racket, 2019-11-03
;; perf-tests.rkt> (bcom-a-lot)
;; cpu time: 1473 real time: 1472 gc time: 49
;; perf-tests.rkt> (bcom-a-lot #:reckless? #t)
;; cpu time: 1262 real time: 1262 gc time: 24
;;; Racket, 2021-07-17
;; perf-tests.rkt> (bcom-a-lot #:reckless? #t)
;; cpu time: 1057 real time: 1058 gc time: 52

;;; Guile, 2022-05-12 
;; scheme@(goblins-perf-test)> ,time (bcom-a-lot)
;; 1.016932s real time, 2.569558s run time.  1.747978s spent in GC.
;; scheme@(goblins-perf-test)> ,time (bcom-a-lot #:reckless? #t)
;; 0.814388s real time, 2.463141s run time.  1.841628s spent in GC.

;; A bunch of actors updating themselves
(define* (bcom-a-lot #:optional [actormap (make-whactormap)]
                     #:key [num-actors 1000]
                     [iterations 1000]
                     [reckless? #f])
  (define* (^incrementing-actor bcom #:optional [i 0])
    (lambda ()
      (bcom (^incrementing-actor bcom (1+ i))
            i)))
  (define i-as
    (let lp ((n num-actors)
             (actors '()))
      (if (zero? n)
          actors
          (lp
           (1- n)
           (cons (actormap-spawn! actormap ^incrementing-actor)
                 actors)))))
  (repeat
   iterations
   (lambda ()
     (actormap-run!
      actormap
      (lambda ()
        (for-each (lambda (i-a) ($ i-a))
                  i-as))
      #:reckless? reckless?))))

;;; Some even simpler tests

;;;; Guile, 2022-05-12
;; scheme@(goblins-perf-test)> ,time (many-simple-calls 1000000)
;; ;; 0.244962s real time, 0.244937s run time.  0.000000s spent in GC.
(define* (many-simple-calls #:optional (n 1000000))
  (actormap-run!
   (make-actormap)
   (lambda ()
     (define friend (spawn ^simple-actor))
     (repeat
      n
      (lambda ()
        ($ friend))))))

;;;; Demonstrating that spawn-named is way cheaper...

;;;; Guile, 2022-05-12
;; scheme@(goblins-perf-test)> ,time (many-spawn)
;; 4.640551s real time, 13.264281s run time.  10.242861s spent in GC.
;; scheme@(goblins-perf-test)> ,time (many-spawn-named)
;; 0.765582s real time, 2.264451s run time.  1.753878s spent in GC.
(define* (many-spawn #:optional (n 100000))
  (actormap-run!
   (make-actormap)
   (lambda ()
     (repeat
      n
      (lambda ()
        (spawn ^simple-actor))))))
(define* (many-spawn-named #:optional (n 100000))
  (actormap-run!
   (make-actormap)
   (lambda ()
     (repeat
      n
      (lambda ()
        (spawn-named 'foo ^simple-actor))))))



