;;; Copyright 2020-2021 Christine Lemmer-Webber
;;; Copyright 2023 David Thompson
;;; Copyright 2023 Juliana Sims
;;; Copyright 2025 Jessica Tallon

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

(define-module (goblins actor-lib on)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins core)
  #:use-module (ice-9 match)
  #:export (let-on
            let*-on
            on-each
            on-map
            on-filter
            on-match))

(define-syntax let-on
  ;;; Concurrently evaluate and resolve each EXP, binding it to VAR
  ;;; before evaluating BODY in an environment where each VAR is bound.
  (syntax-rules ()
    ((_ ((var exp)) body ...)
     (on exp (lambda (var) body ...) #:promise? #t))
    ((_ ((var exp) ...) body ...)
     (on (all-of exp ...)
         (match-lambda
           ((var ...)
            body ...))
         #:promise? #t))))

(define-syntax let*-on
  ;;; Sequentially evaluate and resolve each EXP in an environment
  ;;; where all preceding VARs are bound before binding them to their
  ;;; corresponding VARs, then evaluate BODY in an environment where
  ;;; each VAR is bound.
  (syntax-rules ()
    ((_ ((var exp)) body ...)
     (on exp (lambda (var) body ...) #:promise? #t))
    ((_ ((var exp) . rest) body ...)
     (on exp
         (lambda (var)
           (let*-on rest body ...))
         #:promise? #t))))

(define (on-each proc vow)
  (on vow
      (lambda (result)
        (for-each proc result))))

(define (on-map proc vow)
  (let-on ((result vow))
    (map proc result)))

(define (on-filter pred vow)
  (let-on ((result vow))
    (filter pred result)))

(define-syntax-rule (on-match vow match-clause ...)
  (on vow
      (match-lambda
        match-clause ...)
      #:promise? #t))
