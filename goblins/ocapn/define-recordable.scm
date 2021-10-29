;; Copyright (C) 2013, 2014, 2015 Free Software Foundation, Inc.
;; Copyright (C) 2019-2021 Christine Lemmer-Webber

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

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (goblins ocapn define-recordable)
  #:use-module (syrup)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (define-recordable))

;; Borrowed and modified from the module (language cps)
;; So, LGPLv3+ on this one I guess

;;; This defines a record that can also be serialized into a datastructure.
;;; Also defines marshall::<name> and unmarshall::<name> procedures 
(define-syntax define-recordable
  (lambda (x)
    (define (id-append ctx . syms)
      (datum->syntax ctx (apply symbol-append (map syntax->datum syms))))
    (syntax-case x ()
      ((_ name (field ...))
       (and (identifier? #'name) (and-map identifier? #'(field ...)))
       (with-syntax ((cstr #'name)
                     ;; (cstr (id-append #'name #'make- #'name))
                     (record-name
                      (datum->syntax #'name
                                     (symbol-append '< (syntax->datum #'name) '>)))
                     (pred (id-append #'name #'name #'?))
                     (marshall (id-append #'name #'marshall:: #'name))
                     (unmarshall (id-append #'name #'unmarshall:: #'name))
                     ((getter ...) (map (lambda (f)
                                          (id-append f #'name #'- f))
                                        #'(field ...))))
         (with-ellipsis
          :::
          #'(begin
              (define-record-type record-name
                (cstr field :::)
                pred
                (field getter)
                :::)
              (define marshall
                (cons (match-lambda
                        (($ name field :::)
                         #t)
                        (_ #f))
                      (match-lambda
                        (($ name field :::)
                         (make-syrec* (quote name) field :::)))))
              (define unmarshall
                (cons (lambda (label)
                        (eq? label (quote name)))
                      cstr)))))))))
