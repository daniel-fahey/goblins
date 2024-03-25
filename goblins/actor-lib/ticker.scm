;;; Copyright 2019-2022 Christine Lemmer-Webber
;;; Copyright 2023 Juliana Sims
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
  #:use-module (goblins actor-lib define-actor)
  #:use-module (goblins actor-lib selfish-spawn)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (spawn-ticker ticker-env))

(define (make-to-tick-beh new-ticked)
  (lambda (give-ticky)
    (define ticky
      (spawn ^ticky #f new-ticked))
    (define new-refr
      (give-ticky ticky))
    ($ new-ticked
       (cons (vector new-refr ticky) ($ new-ticked)))
    new-refr))

(define-actor (^ticky bcom dead? new-ticked)
  #:frozen
  (define to-tick
    (make-to-tick-beh new-ticked))
  (methods
   [(die)
    (bcom (^ticky bcom #t new-ticked))]
   [(dead?)
    dead?]
   [to-tick to-tick]))

(define-actor (^ticker bcom current-ticked new-ticked)
  #:frozen
  (define to-tick
    (make-to-tick-beh new-ticked))
  (methods
   [to-tick to-tick]
   [(get-ticked)
    (map (match-lambda
           (#(refr ticky)
            refr))
         current-ticked)]
   
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
      
      ;; Now run all ticked objects and keep the survivors
      (define next-tickers
        (filter (match-lambda
                  [#(ticked-refr ticked-ticky)
                   (and
                    (not ($ ticked-ticky 'dead?))
                    (begin
                      (apply $ ticked-refr args)
                      (not ($ ticked-ticky 'dead?))))])
                updated-ticked))
      (bcom (^ticker bcom next-tickers new-ticked)))]
   ;; Used for collision detection, etc.
   ;; Similar to the above but with a bit of extra overhead to build up
   ;; a value
   [(foldr proc init #:key (include-new? #t))
    ;; Update set of tickers with any that have been
    ;; added since when we last ran
    (define updated-ticked
      (if include-new?
          (append ($ new-ticked) current-ticked)
          current-ticked))
    
    (define fold-result
      (fold-right (match-lambda*
                    [(#(refr ticky) prev)
                     (proc refr prev)])
                  init current-ticked))
    
    ;; filter out the tickers who are dead
    (define next-tickers
      (filter (match-lambda
                (#(refr ticky)
                 (not ($ ticky 'dead?))))
              updated-ticked))
    
    ;; reset new-ticked
    (when include-new?
      ($ new-ticked '()))
    
    ;; return result and become ticker with set of new tickers
    (bcom (^ticker bcom next-tickers new-ticked)
          fold-result)]))  ; return fold result

(define (spawn-ticker)
  "Spawn and return a reference to a Ticker actor which invokes a set of
actors each time its tick method is invoke.

Ticker Methods:
`to-tick give-ticky': Return a procedure accepting a single argument,
a closure over spawning an actor with a Ticky actor as an argument.
`get-ticked': Return a list holding references to each tickable actor.
`tick': Invoke all actors which have a Ticky.
`foldr proc init [#:include-new? #t]': A foldr procedure over the
actors known to the Ticker. If INCLUDE-NEW? is #t, also operate on
actors added since the last tick.

Ticky Methods:
`die': Set `dead/' to #t.
`dead?': Return #t if `die' has been invoked, else #f.
`to-tick': Same as a Ticker's `to-tick'.

Type: -> Ticker"
  (define-cell new-ticked '())
  (spawn ^ticker '() new-ticked))

(define ticker-env
  (make-persistence-env
   `((((goblins actor-lib ticker) ^ticky) ,^ticky)
     (((goblins actor-lib ticker) ^ticker) ,^ticker))
   #:extends cell-env))
