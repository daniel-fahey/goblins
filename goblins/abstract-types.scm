;;; Copyright 2023 Jessica Tallon
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
(define-module (goblins abstract-types)
  #:use-module (srfi srfi-9)
  #:export (zilch
            zilch?

            <tagged>
            make-tagged
            make-tagged*
            tagged?
            tagged-label
            tagged-data))

;; This is both a 2nd and secondary "bottom" or null/void type that's used
;; within CapTP. This works alongside guile's *unspecified* bottom type.
(define (make-zilch)
  (define-record-type <zilch>
    (_make-zilch)
    zilch?)
  (values (_make-zilch) zilch?))

(define-values (zilch zilch?)
  (make-zilch))

(define-record-type <tagged>
  (make-tagged label data)
  tagged?
  (label tagged-label)
  (data tagged-data))

(define (make-tagged* label . args)
  (make-tagged label args))
