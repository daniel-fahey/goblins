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
  #:export (^cell
            cell->read-only
            cell->write-only
            define-cell))

;;; Cells
;;; =====

;; A simple turn-mutable cell

;; Constructor for a cell.  Takes an optional initial value, defaults
;; to false.
(define* (^cell bcom #:optional [val #f])
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

(define (cell->read-only cell)
  "Create a read-only reference to CELL.

Type: Cell -> ROCell"
  (define (^ro-cell bcom)
    (lambda () ($ cell)))
  (spawn ^ro-cell))

(define (cell->write-only cell)
  "Create a write-only reference to CELL.

Type: Cell -> WOCell"
  (define (^wo-cell bcom)
    (lambda (new-val) ($ cell new-val)))
  (spawn ^wo-cell))

(define-syntax define-cell
  ;;; Define a Cell using standard Scheme define syntax.
  (syntax-rules ()
    [(_ id)
     (define id
       (spawn-named 'id ^cell))]
    [(_ id val)
     (define id
       (spawn-named 'id ^cell val))]))
