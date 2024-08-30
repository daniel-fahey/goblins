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

(define-module (tests test-migrations)
  #:use-module (goblins migrations)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64))

(test-begin "test-migrations")

(define test-migration
  (migrations
   [(1 foo) (list (list '0->1 foo))]
   [(2 foo) (list (list '1->2 foo))]
   [(3 bar) (list bar '2->3)]
   [(4 bar baz) (list bar baz '3->4)]))

(test-assert "Check we can run all the migrations and get back result"
  (let-values (((new-version new-roots) (test-migration 0 'i-am-starting-value)))
    (match new-roots
      [(('1->2 ('0->1 'i-am-starting-value)) '2->3 '3->4) (= new-version 4)])))

;; Try dropping a migration entirely
(define migrations-without-0
  (migrations
   [(2 foo) (list (list '1->2 foo))]
   [(3 bar) (list bar '2->3)]
   [(4 bar baz)
    (list bar baz '3->4)]))

(test-error "Check trying to migrate from unsupported version errors"
 #t
 (let-values (((new-version new-roots) (migrations-without-0 0 'something)))
   new-roots))

(test-end "test-migrations")
