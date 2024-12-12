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
  (export define-applicable-record-type)
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
      (define-syntax define-procedure-accessors
        (lambda (stx)
          (syntax-case stx ()
            ((_ (procedure procedure-getter))
             #'(define procedure-getter hoot:applicable-record-procedure))
            ((_ (procedure procedure-getter procedure-setter))
             #'(begin
                 (define procedure-getter hoot:applicable-record-procedure)
                 (define procedure-setter hoot:set-applicable-record-procedure!))))))
      (define-syntax define-applicable-record-type
        (lambda (stx)
          (syntax-case stx ()
            ((_ name constructor predicate (procedure-accessors ...) fields  ...)
             #'(begin
               (define-procedure-accessors (procedure-accessors ...))
               (hoot:define-record-type name
                                        #:parent hoot:<applicable-record>
                                        constructor
                                        predicate
                                        fields ...)))))))
     (guile
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
                           ((index ...) (iota (length #'(field ...)))))
               #'(begin
                   (define name
                     (make-struct/no-tail <applicable-struct-vtable> 'layout))
                   (define (predicate obj)
                     (and (struct? obj) (eq? (struct-vtable obj) name)))
                   (define (constructor fields ...)
                     (make-struct/no-tail name fields ...))
                   (define-accessors name predicate index accessor ...)
                   ...))))))))))
