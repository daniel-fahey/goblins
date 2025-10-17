;;; Copyright 2025 Juliana Sims
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

(define-module (tests actor-lib test-methods)
  #:use-module (goblins actor-lib methods)
  #:use-module (srfi srfi-64))

(test-group "test-methods"

  (test-equal "no arguments methods work"
    'no-arguments
    ((methods ((no-args) 'no-arguments)) 'no-args))

  (test-equal "one argument methods work"
    'one-argument
    ((methods ((one-arg arg) arg)) 'one-arg 'one-argument))

  (test-equal "dotted rest arguments methods work"
    (list 'rest 'arguments)
    ((methods ((dotted . args) args)) 'dotted 'rest 'arguments))

  (test-equal "procedure methods work"
    'procedure
    ((methods (proc (lambda () 'procedure))) 'proc))

  (test-equal "optional arguments methods work"
    'optional-argument
    ((methods ((opt #:optional arg) arg)) 'opt 'optional-argument))

  (test-equal "keyword arguments methods work"
    'keyword-argument
    ((methods ((keyword #:key karg) karg)) 'keyword #:karg 'keyword-argument))

  (test-equal "rest arguments methods work"
    (list 'rest 'arguments)
    ((methods ((rest #:rest r) r)) 'rest 'rest 'arguments))

  (test-equal "all lambda* features methods work"
    (list 'argument 'optional-argument 'keyword-argument
          (list #:karg 'keyword-argument 'rest 'arguments))
    ((methods ((all arg #:optional opt #:key karg #:rest r)
                (list arg opt karg r)))
     'all
     'argument 'optional-argument #:karg 'keyword-argument
     'rest 'arguments)))
