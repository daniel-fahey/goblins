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


(define-module (goblins actor-lib ticker)
  #:use-module (goblins core)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (spawn-ticker))

(define (spawn-ticker)
  (define-cell new-ticked
    '())

  (define (to-tick give-ticky)
    (define ticky
      (spawn ^ticky #f))
    (define new-refr
      (give-ticky ticky))
    ($ new-ticked
       (cons (vector new-refr ticky) ($ new-ticked)))
    new-refr)

  (define (^ticky bcom dead?)
    (methods
     [(die)
      (bcom (^ticky bcom #t))]
     [(dead?)
      dead?]
     [to-tick to-tick]))

  (define (^ticker bcom current-ticked)
    (methods
     [to-tick to-tick]

     ;; This wonky looking procedure actually does the ticking.
     ;; We apply any arguments given to the tick method to all
     ;; refrs that aren't dead according to their ticky.  And if
     ;; they're still not dead, then we queue them up for next
     ;; time.
     [tick
      (lambda args
        ;; Update set of tickers with any that have been
        ;; added since when we last ran
        (define updated-ticked
          (append ($ new-ticked) current-ticked))
        ;; reset new-ticked
        ($ new-ticked '())

        ;; Now run all ticked objects
        ;; (@@: The natural iteration makes this not so easy to read.
        ;;   Maybe it's worth a rewrite for cleanliness?  Dunno.)
        (define next-tickers
          (let lp ([to-tick updated-ticked])
            (match to-tick
              ['() '()]
              [(this-ticked . tick-rest)
               (match this-ticked
                 [#(ticked-refr ticked-ticky)
                  (if ($ ticked-ticky 'dead?)
                      ;; continue, it's dead
                      (lp tick-rest)
                      ;; otherwise, let's tick it
                      (begin
                        (apply $ ticked-refr args)
                        (if ($ ticked-ticky 'dead?)
                            ;; well it wasn't dead before, but it is now
                            (lp tick-rest)
                            ;; ok it's dead now too
                            (cons this-ticked
                                  (lp tick-rest)))))])])))
        (bcom (^ticker bcom next-tickers)))]
     ;; Used for collision detection, etc.
     ;; Note that this does NOT end up including any updates that have
     ;; come in within the interim, but arguably should, and also should
     ;; probably remove dead things too.  In other words, we should
     ;; probably merge some of this behavior with the previous method!
     [(foldr proc init)
      (fold-right (match-lambda*
                    [(#(refr ticky) prev)
                     ;; skip if dead (probably from a previous foldr)
                     (if ($ ticky 'dead?)
                         prev
                         (proc refr prev))])
                  init current-ticked)]))

  (spawn ^ticker '()))
