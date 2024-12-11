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

(define-library (goblins utils define-applicable-record-type)
  (import (goblins utils assert-type))
  (export define-applicable-record-type
          applicable-record-procedure
          set-applicable-record-procedure!)
  (cond-expand
   (guile
    (import (guile)
            (ice-9 match)
            (srfi srfi-9)))
   (hoot
    (import (guile)
            (ice-9 match)
            (prefix (hoot records) hoot:))))

  (begin
    (cond-expand
     (hoot
      (define applicable-record? hoot:applicable-record?)
      (define applicable-record-procedure hoot:applicable-record-procedure)
      (define (set-applicable-record-procedure! obj new-procedure)
        (error "Setting the applicable record procedure is not supported under hoot."))
      (define-syntax-rule (define-applicable-record-type name constructor predicate fields ...)
        (hoot:define-record-type name
          #:parent hoot:<applicable-record>
          constructor
          predicate
          fields ...)))
     (guile
      ;; These are normally defined by hoot, but when we're just on guile
      ;; they don't exist so we need to define them here...
      (define (applicable-record? maybe)
        (and (procedure? maybe) (struct? maybe)))
      (define (applicable-record-procedure obj)
        (assert-type obj applicable-record?)
        (struct-ref obj 0))
      (define (set-applicable-record-procedure! obj new-procedure)
        (assert-type obj applicable-record?)
        (struct-set! obj 0 new-procedure))
      (define-syntax-rule (define-getter name rtd predicate index)
        (define (name obj)
          (assert-type obj predicate)
          (struct-ref obj index)))
      (define-syntax-rule (define-setter name rtd predicate index)
        (define (name obj new)
          (assert-type obj predicate)
          (struct-set! obj index new)))
      (define-syntax define-accessors
        (syntax-rules ()
          ((_ rtd predicate index getter setter)
           (begin
             (define-getter getter rtd predicate index)
             (define-setter setter rtd predicate index)))
          ((_ rtd predicate index getter)
           (define-getter getter rtd predicate index))
          ((_ rtd)
           (values))))
      (define-syntax define-applicable-record-type
        (lambda (stx)
          (define (make-layout lst)
            (apply symbol-append 'pw (map (const 'pw) lst)))
          (syntax-case stx ()
            ((_ name (constructor fields ...) predicate (field accessor ...) ...)
             (with-syntax ((layout (datum->syntax stx (make-layout #'(field ...))))
                           ((index ...) (iota (length #'(field ...)) 1)))
               #'(begin
                   (define name
                     (make-struct/no-tail <applicable-struct-vtable> 'layout))
                   (define (predicate obj)
                     (and (struct? obj) (eq? (struct-vtable obj) name)))
                   (define (constructor fields ...)
                     (make-struct/no-tail name fields ...))
                   (define-accessors name predicate index accessor ...)
                   ...))))))))))
