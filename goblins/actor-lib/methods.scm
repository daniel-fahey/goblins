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
  #:export (methods extend-methods))

(define-syntax expand-method-defn
  (syntax-rules ()
    ((_ ((method-name method-args ...) body ...))
     (let ((method-name (lambda* (method-args ...) body ...)))
       (cons 'method-name method-name)))
    ((_ (method-name method-expr))
     (cons 'method-name method-expr))))

(define-syntax-rule (methods* fallback method-defn ...)
  (let* ((all-methods (list (expand-method-defn method-defn) ...))
         (this-fallback fallback)
         (methods
          (lambda (method . args)
            (let ((found-method (assq-ref all-methods method)))
              (if found-method
                  (apply found-method args)
                  (apply fallback method args))))))
    methods))

(define (no-such-method method . args)
  (error "No such method" method args))

(define-syntax-rule (methods method-defns ...)
  (methods* no-such-method method-defns ...))

(define (extend-actor extends-actor)
  (lambda (method . args)
    (apply $C extends-actor method args)))

(define-syntax-rule (extend-methods extends method-defns ...)
  (methods* (match extends
              ;; we extend procedures as-is
              ((? procedure?) extends)
              ;; but wrap actors in procedure that calls them
              ((? live-refr?)
               (extend-actor extends))
              (#f no-such-method))
            method-defns ...))
