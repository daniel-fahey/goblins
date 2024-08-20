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
  #:use-module (srfi srfi-64))

(test-begin "test-migrations")

(define test-migration
  (migrations
   [(0 foo) `(('0->1 ,foo))]
   [(1 foo) `(('1->2 ,foo))]
   [(2 bar) (list bar '2->3)]
   [(3 bar baz)
    (list bar baz '3->4)]))

(test-assert "Check we can run all the migrations and get back result"
  (match (test-migration 0 4 'i-am-starting-value)
    [(('1->2 ('0->1 'i-am-starting-value)) '2->3 '3->4) #t]))

;; Try dropping a migration entirely
(define migrations-without-0
  (migrations
   [(1 foo) `(('1->2 ,foo))]
   [(2 bar) (list bar '2->3)]
   [(3 bar baz)
    (list bar baz '3->4)]))

(test-error "Check trying to migrate from unsupported version errors"
 #t
 (migrations-without-0 0 4 'something))

;; Check we can run just one migration
(test-assert "Check we can just run a single migration"
  (match (test-migration 0 1 'hello)
    [('0->1 'hello) #t]))

(test-end "test-migrations")