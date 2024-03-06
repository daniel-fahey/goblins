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
                          set!-redefinable-object-constructor))
  #:export (define-actor define-hackable))


(define-syntax-rule (define-redefinable-object name proc)
  (define name
    (if (defined? 'name)
	;; We've already defined this, just update the constructor refr
	(begin
	  (set!-redefinable-object-constructor name proc)
	  name)
	;; First time, lets define it.
	(make-redefinable-object proc))))

(define-syntax define-actor
  (lambda (stx)
    (define* (args->arg-names args #:key [is-keyword? #f])
      (if (null? args)
	  '()
	  (syntax-case (car args) ()
	    [#:key (args->arg-names (cdr args) #:is-keyword? #t)]
	    [#:optional (args->arg-names (cdr args) #:is-keyword? #f)]
	    [(identifier default-value)
	     (let ((rest (args->arg-names (cdr args) #:is-keyword? is-keyword?)))
	       (if is-keyword?
		   (cons
		    (datum->syntax #'identifier (symbol->keyword (syntax->datum #'identifier)))
		    (cons #'identifier rest))
		   (cons #'identifier rest)))]
	    [identifier
	     (let ((rest (args->arg-names (cdr args) #:is-keyword? is-keyword?)))
	       (if is-keyword?
		   (cons
		    (datum->syntax #'identifier (symbol->keyword (syntax->datum #'identifier)))
		    (cons #'identifier rest))
		   (cons #'identifier rest)))])))

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
