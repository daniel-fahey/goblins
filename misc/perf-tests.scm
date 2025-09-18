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

(use-modules (goblins core)
             (ice-9 match)
             (ice-9 curried-definitions))


;; This timing report is more or less lifted form guile's `,time' meta command
;; located in guile/module/system/repl/command.scm
;; commit: d8df317bafcdd9fcfebb636433c4871f2fab28b2
;; License: LGPL v3.
(define (time-it thunk)
  (let* ((gc-start (gc-run-time))
         (real-start (get-internal-real-time))
         (run-start (get-internal-run-time))
         (result (thunk))
         (run-end (get-internal-run-time))
         (real-end (get-internal-real-time))
         (gc-end (gc-run-time)))
    (define (diff start end)
      (/ (- end start) 1.0 internal-time-units-per-second))
    (values
     (diff real-start real-end)
     (diff run-start run-end)
     (diff gc-start gc-end))))
;; -- END ,time meta command ---

(define* (output-metric id help thunk #:key (unit "seconds") (type "counter"))
  "Writes metrics in the open metrics format"
  (define-values (real run gc)
    (time-it thunk))
  (format #t "# TYPE ~a ~a\n" id type)
  (format #t "# UNIT ~a ~a\n" id unit)
  (format #t "# HELP ~a ~a\n" id help)
  (format #t "~a-real ~a\n" id real)
  (format #t "~a-run ~a\n" id run)
  (format #t "~a-gc ~a\n" id gc))

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

(define (^simple-actor bcom)
  (lambda ()
    'hello))

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


;;; Racket, 2019-10-29
;; perf-tests.rkt> (set!-a-lot)
;; cpu time: 1106 real time: 1105 gc time: 13

;;; Guile, 2023-01-07
;; scheme@(goblins-perf-test)> ,time (set!-a-lot)
;; 0.478929s real time, 1.266432s run time.  0.887123s spent in GC.

;; So, Guile seems faster on this one, again excluding GC.
;; (Why there's so much GC still, I have no idea?  Worth investigating...)

(define* (set!-a-lot #:optional [actormap (make-whactormap)]
                     #:key [num-actors 1000]
                     [iterations 1000])
  (define (^incrementing-actor bcom)
    (define i 0)
    (lambda ()
      (define old-i i)
      (set! i (1+ i))
      old-i))
  (define i-as
    (do ((i 0 (1+ i))
         (l '() (cons (actormap-spawn! actormap ^incrementing-actor)
                      l)))
        ((= i num-actors) l)))
  (do ((i 0 (1+ i)))
      ((= i iterations))
    (actormap-run!
     actormap
     (lambda ()
       (for-each $ i-as)))))

;;; Racket: 2021-07-17
;; perf-tests.rkt> (send-a-lot)
;; cpu time: 12261 real time: 12261 gc time: 257
;; perf-tests.rkt> (send-a-lot #:promise? #t)
;; cpu time: 50629 real time: 50631 gc time: 2482
;;
;; So that's about 81.5k messages per second when using <-np,
;; about 20k messages per second when using <-
;;
;; perf-tests.rkt> (send-a-lot #:spawn-cell? #t)
;; cpu time: 19289 real time: 19289 gc time: 550
;;
;; So, not that much bigger of an increase when "merely" spawning a
;; cell.  Promises with <- seem to be expensive.  I do wonder if
;; there's room for optimization somewhere.

;;; Guile: 2023-01-07
;;
;; scheme@(goblins-perf-test)> ,time (send-a-lot)
;; $8 = #(ok #<unspecified>)
;; 3.361616s real time, 13.404054s run time.  11.498390s spent in GC.
;;
;; scheme@(goblins-perf-test)> ,time (send-a-lot #:promise? #t)
;; $9 = #(ok #<unspecified>)
;; 84.676490s real time, 304.517632s run time.  253.822021s spent in GC.
;;
;; scheme@(goblins-perf-test)> ,time (send-a-lot #:spawn-cell? #t)
;; $10 = #(ok #<unspecified>)
;; 59.090220s real time, 178.360456s run time.  141.607579s spent in GC.

;; Whoa so ok, these are a lot worse off in Guile land than they were
;; in Racket land it seems...

(define* (send-a-lot #:key [promise? #f]
                     [iterations 1000000]
                     [spawn-cell? #f])
  (define am (make-actormap))
  (define (^cell bcom val)
    (case-lambda
      [() val]
      [(v) (bcom (^cell bcom v))]))
  (actormap-churn-run
   am
   (lambda ()
     (define ((^self-looper bcom) n)
       (when spawn-cell?
         (spawn ^cell 8))
       (when (not (zero? n))
         ((if promise? <- <-np)
          self-looper (1- n)))
       'another-one-done)
     (define self-looper (spawn ^self-looper))
     (<-np self-looper iterations))))

(define (main . args)
  (output-metric
   "perf-synchronous-call-a-lot"
   "Calling an actor with $ many times"
   call-a-lot)
  (output-metric
   "perf-bcom-a-lot"
   "An actor using bcom many times"
   bcom-a-lot)
  (output-metric
   "perf-set-a-lot"
   "An actor using set! many times"
   set!-a-lot)
  (output-metric
   "perf-send-a-lot"
   "Invoking an actor with <-np (send message without result) many times"
   send-a-lot)

  (format #t "# EOF\n"))
