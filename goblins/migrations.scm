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
  #:use-module (srfi srfi-1)
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
       (every exact-integer? (syntax->datum #'(from-version ...)))
       (let-values (((min max) (find-supported-versions #'(from-version ...))))
         (with-syntax ((min min)
                       (max max))
           #`(lambda (init-version . init-data)
               (define (unsupported? version)
                 (< version min))
               (define (migrator version migration-data)
                 (match version
                   [(? unsupported? old-version)
                    ;; We want to define an exception type for all persistence related errors, so they can be caught.
                    (error (format #f "Data version ~a is too old, minimum supported version is ~a"
                                   old-version min))]
                   [from-version
                    (match migration-data
                      [(data ...) body ...])]
                   ...))
               (let lp ((current-version init-version)
                        (current-data init-data))
                 (let ((target-version (+ current-version 1)))
                   (if (< max target-version)
                       (values current-version current-data)
                       (lp target-version
                           (migrator target-version current-data))))))))])))
