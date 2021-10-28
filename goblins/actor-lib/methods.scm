;;; Copyright 2019-2021 Christine Lemmer-Webber
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

(define-module (goblins actor-lib methods)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module ((goblins core) #:renamer (lambda (x) (if (eq? x '$) '$C x)))
  #:export (methods* methods extend-methods))

;; Half-implemented version of methods
(define-syntax-rule (methods*
                     ((method-name method-args ...) body ...) ...
                     fallback)
  (let ((these-methods
         (let* ((method-name
                 (lambda (method-args ...)
                   body ...))
                ...
                (all-methods (list (cons 'method-name method-name) ...))
                (this-fallback fallback))
           (lambda (method . args)
             (let ((found-method (assq-ref all-methods method)))
               (if found-method
                   (apply found-method args)
                   (apply fallback method args)))))))
    (set-procedure-property! these-methods 'name 'methods)
    these-methods))

(define (no-such-method method . args)
  (error "No such method" method args))

(define-syntax-rule (methods method-defns ...)
  (methods* method-defns ... no-such-method))

(define (extend-actor extends-actor)
  (lambda (method . args)
    (apply $C extends-actor method args)))

(define-syntax-rule (extend-methods method-defns ... extends)
  (methods method-defns ... (extend-actor extends)))
