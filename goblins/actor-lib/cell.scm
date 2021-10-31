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

(define-module (goblins actor-lib cell)
  #:use-module (goblins core)
  #:export (^cell
            spawn-cell
            cell->read-only
            cell->write-only))

;;; Cells
;;; =====

;; A simple turn-mutable cell

;; Constructor for a cell.  Takes an optional initial value, defaults
;; to false.
(define* (^cell bcom #:optional [val #f])
  (case-lambda
    ;; Called with no arguments; return the current value
    [() val]
    ;; Called with one argument, we become a version of ourselves
    ;; with this new value
    [(new-val)
     (bcom (^cell bcom new-val))]))

(define* (spawn-cell #:optional [val #f])
  (spawn ^cell val))

(define (cell->read-only cell)
  (define (^ro-cell bcom)
    (lambda () ($ cell)))
  (spawn ^ro-cell))

(define (cell->write-only cell)
  (define (^wo-cell bcom)
    (lambda (new-val) ($ cell new-val)))
  (spawn ^wo-cell))

(define-syntax define-cell
  (lambda (stx)
    (syntax-rules ()
      [(_ id)
       (define id
         (spawn (procedure-rename ^cell 'id)))]
      [(_ id val)
       (define id
         (spawn (procedure-rename ^cell 'id) val))])))
