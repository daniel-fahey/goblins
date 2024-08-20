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

(define-syntax-rule (migrations ((from-version root ...) body ...) ...)
  (lambda (prev-version . roots)
    (define provided-migrations
      (list (cons from-version (lambda (root ...) body ...))
            ...))

    (let lp ((current-version prev-version)
             (current-roots roots)
             (migrations provided-migrations))
      (if (null? migrations)
          (values current-version current-roots)
          (let* ((current-migration (car migrations))
                 (migration-version (car current-migration))
                 (migrator (cdr current-migration)))
            (cond ((< migration-version current-version) (lp (+ current-version 1) current-roots (cdr migrations)))
                  ((= migration-version current-version)
                   (lp (+ current-version 1)
                       (apply migrator current-roots)
                       (cdr migrations)))
                  ((> migration-version current-version)
                   (error (format #f "Migration for version ~a not supported by this migrator" current-version)))))))))
