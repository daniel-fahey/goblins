;;; Copyright 2019-2023 Christine Lemmer-Webber
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

(define-module (goblins actor-lib pushdown)
  #:use-module (goblins core)
  #:use-module (goblins define-actor)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib opportunistic)
  #:use-module (goblins actor-lib methods)
  #:use-module (ice-9 match)
  #:export (spawn-pushdown-pair pushdown-env))

(define-actor (^pd-stack _bcom stack)
  #:frozen
  (methods
   ((push refr)
    ;; Add to the stack
    ($ stack (cons refr ($ stack))))
   ((spawn-push constructor #:rest args)
    (define cur-stack
      ($ stack))
    (define stack-top
      (match cur-stack
        [(stack-top . rest-stack)
         stack-top]
        ['() #f]))
    (define new-refr
      (apply spawn constructor stack-top args))
    ($ stack (cons new-refr cur-stack))
    new-refr)
   ((pop)
    (match ($ stack)
      [(stack-top . rest-stack)
       ;; set stack to the remaining value
       ($ stack rest-stack)
       ;; return the top value
       stack-top]
      ['() (error "Empty stack")]))
   ((empty?)
    (null? ($ stack)))))

(define-actor (^pd-forwarder _bcom stack)
  #:frozen
  (lambda args
    (match ($ stack)
      [(stack-top . rest-stack)
       (apply run-$/<- stack-top args)])))

(define pushdown-env
  (make-persistence-env
   `((((goblins actor-lib pushdown) ^pd-stack) ,^pd-stack)
     (((goblins actor-lib pushdown) ^pd-forwarder) ,^pd-forwarder))
   #:extends cell-env))
  
(define* (spawn-pushdown-pair #:optional initial-refr)
  "Spawn a pair which constitute a pushdown automata, a Pd-Stack for the stack
and a Pd-Forwarder to forward messages to the current top of the stack.

Pd-Stack Methods:
`push refr': Add REFR to the stack.
`spawn-push constructor args ...': Spawn the actor constructed by CONSTRUCTOR,
passing the current top of the stack and the arguments ARGS to the constructor;
then add the new actor to the top of the stack. Return a reference to the new actor.
`pop': Remove and return the top empty of the stack, or error if empty.
`empty?': Return #t if the stack is empty, else #f.

Type: (Optional Actor) -> (Values Pd-Stack Pd-Forwarder)"
  (let ((stack (spawn-named 'stack
                            ^cell
                            (if initial-refr
                                (list initial-refr)
                                '()))))
    (values (spawn ^pd-stack stack)
            (spawn ^pd-forwarder stack))))
