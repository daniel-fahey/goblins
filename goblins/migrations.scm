;;; Copyright 2024 Jessica Tallon
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

(define-module (goblins migrations)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:export (migrations))

(define-syntax migrations
  (lambda (stx)
    (define (find-supported-versions versions-stx)
      (define versions (map syntax->datum versions-stx))
      (define sorted (sort versions <))
      (match sorted
        [(version) (values version version)]
        [(lowest-version rest ... highest-version)
         (values lowest-version highest-version)]))

    (syntax-case stx ()
      [(_ ((from-version data ...) body ...) ...)
       (let-values (((min max) (find-supported-versions #'(from-version ...))))
         (with-syntax ((min min)
                       (max max))
           #`(lambda (init-version . init-data)
               (define (unsupported? version)
                 (and (number? version) (< version min)))
               (define migrator
                 (match-lambda
                   [((? unsupported? old-version) unsupported-data :::)
                    (error (format #f "Data version ~a is too old, minimum supported version is ~a"
                                   old-version min))]
                   [(from-version data ...) body ...]
                   ...))
               (let lp ((current-version init-version)
                        (current-data init-data))
                 (if (< max (+ current-version 1))
                     (values current-version current-data)
                     (lp (+ 1 current-version)
                         (migrator (cons (+ current-version 1) current-data))))))))])))
