;;; Copyright 2019-2021 Christine Lemmer-Webber
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

(define-module (goblins actor-lib cell)
  #:use-module (goblins core)
  #:use-module (goblins actor-lib define-actor)
  #:export (^cell
            cell->read-only
            cell->write-only
            define-cell
	    cell-env))

;;; Cells
;;; =====

;; A simple turn-mutable cell
(define-actor (^cell bcom #:optional val)
  "Construct a Cell taking an optional VAL which defaults to #f.

The constructed cell can be invoked without an argument, which will return VAL;
or with an argument, resulting in the cell becoming a version with the argument
as VAL."
  (case-lambda
    ;; Called with no arguments; return the current value
    [() val]
    ;; Called with one argument, we become a version of ourselves
    ;; with this new value
    [(new-val)
     (bcom (^cell bcom new-val))]))

(define-actor (^ro-cell _bcom cell)
  "A cell facet that only allows reading"
  (lambda ()
    ($ cell)))
(define (cell->read-only cell)
  "Create a read-only reference to CELL.

Type: Cell -> ROCell"
  (spawn ^ro-cell cell))

(define-actor (^wo-cell _bcom cell)
  "A cell facet that only allows writing"
  (lambda (new-val)
    ($ cell new-val)))
(define (cell->write-only cell)
  "Create a write-only reference to CELL.

Type: Cell -> WOCell"
  (spawn ^wo-cell cell))

(define cell-env
  (make-persistence-env
   `((((goblins actor-lib cell) ^cell) ,^cell)
     (((goblins actor-lib cell) ^ro-cell) ,^ro-cell)
     (((goblins actor-lib cell) ^wo-cell) ,^wo-cell))))

(define-syntax define-cell
  ;;; Define a Cell using standard Scheme define syntax.
  (syntax-rules ()
    [(_ id)
     (define id
       (spawn-named 'id ^cell))]
    [(_ id val)
     (define id
       (spawn-named 'id ^cell val))]))
