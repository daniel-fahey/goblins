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
;;;
(define-module (goblins actor-lib define-actor)
  #:use-module ((goblins core-types)
                #:select (portraitize
                          make-redefinable-object
			  redefinable-object?
                          set-redefinable-object-constructor!))
  #:export (define-actor define-hackable))


(define-syntax-rule (define-redefinable-object name proc)
  (define name
    (if (defined? 'name)
	;; We've already defined this, just update the constructor refr
	(begin
	  (set-redefinable-object-constructor! name proc)
	  name)
	;; First time, lets define it.
	(make-redefinable-object proc))))

(define-syntax define-actor
  (lambda (stx)
    (define* (args->arg-names args #:key is-keyword?)
      (define (identifier->keyword id)
	"Convert identifier to keyword for identifier. (e.g. 'name' -> #:name"
	(datum->syntax #f (symbol->keyword (syntax->datum id))))
      (define (cons-id id lst)
	"Add the provided ID to the list of arguments"
	;; If we're handling keyword arguments, add the keyword for
	;; the identifier as well as the identifier itself so that
	;; when applied it works at as e.g. (#:name name)
	;; Otherwise just add the id.
	(if is-keyword?
	    (cons* (identifier->keyword id) id lst)
	    (cons id lst)))

      ;; Go through each argument to the actor pulling out the
      ;; identifier only (e.g. skip #:key, #:optional, default values,
      ;; etc.). If it's a keyword argument we want to include the
      ;; identifier's keyword and the identifier itself.
      (syntax-case args ()
	(() '())
	((#:key . rest)
	 (args->arg-names #'rest #:is-keyword? #t))
	((#:optional . rest)
	 (args->arg-names #'rest #:is-keyword? #f))
	(((id default) . rest)
	 (identifier? #'id)
	 (cons-id #'id (args->arg-names #'rest #:is-keyword? is-keyword?)))
	((id . rest)
	 (identifier? #'id)
	 (cons-id #'id (args->arg-names #'rest #:is-keyword? is-keyword?)))))
    (syntax-case stx ()
      [(_ (constructor-id bcom arg ...) body ...)
       (with-syntax (((arg-name ...) (args->arg-names #'(arg ...))))
	 #'(define-redefinable-object
	     constructor-id
	     (lambda* (bcom arg ...)
	       (define (main-beh)
		 body ...)
	       (define (self-portrait)
		 (list arg-name ...))
	       (portraitize (main-beh) self-portrait))))])))

(define-syntax-rule (define-hackable (constructor-id bcom args ...) body ...)
  (define-redefinable-object constructor-id
    (let ((constructor-id
	   (lambda (bcom args ...)
	     body ...)))
      constructor-id)))
