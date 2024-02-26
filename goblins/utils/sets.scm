;;; Copyright 2024 Christine Lemmer-Webber
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

(define-module (goblins utils sets)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-set
            set?
            set-add
            set-remove
            set-fold
            set->list
            set-member?))

;;; An extremely meh implementation of sets
;;; TODO: make and replace with "gsets"
(define-record-type <set>
  (_make-set ht)
  set?
  (ht _set-ht))

(define (print-set set port)
  (define items
    (vhash-fold
     (lambda (k _v prev)
       (cons k prev))
     '()
     (_set-ht set)))
  (format port "#<set ~a>" items))

(set-record-type-printer! <set> print-set)

(define (make-set . items)
  (define vh
    (fold
     (lambda (item vh)
       (vhash-consq item #t vh))
     vlist-null items))
  (_make-set vh))

(define (set-add set item)
  (_make-set (vhash-cons item #t (_set-ht set))))

(define (set-remove set item)
  (_make-set (vhash-delete (_set-ht set) item)))

(define (set-fold proc init set)
  (vhash-fold
   (lambda (key _val prev)
     (proc key prev))
   init
   (_set-ht set)))

(define (set->list set)
  (vhash-fold
   (lambda (key _val prev)
     (cons key prev))
   '()
   (_set-ht set)))

(define (set-member? set key)
  (match (vhash-assoc key (_set-ht set))
    [(_val . #t)
     #t]
    [#f #f]))
