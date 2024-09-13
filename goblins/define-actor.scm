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
(define-module (goblins define-actor)
  #:use-module ((goblins) #:select (spawn))
  #:use-module ((goblins core-types)
                #:select (portraitize
                          make-redefinable-object
                          redefinable-object?
                          set-redefinable-object-constructor!
                          set-redefinable-object-rehydrator!
                          versioned
                          versioned-data?
                          versioned-data-version))
  #:use-module (srfi srfi-71)   ; extended let for multiple values
  #:export (define-actor define-hackable))


(define-syntax-rule (define-redefinable-object-with-rehydrator name proc rehydrator)
  (define name
    (if (and (defined? 'name) (redefinable-object? name))
        ;; We've already defined this, just update the constructor refr
        (begin
          (set-redefinable-object-constructor! name proc)
          (set-redefinable-object-rehydrator! name rehydrator)
          name)
        ;; First time (or currently not a redefinable object),
        ;; lets define it.
        (make-redefinable-object proc rehydrator))))

(define-syntax-rule (define-redefinable-object name proc)
  (define-redefinable-object-with-rehydrator name proc #f))

(define (raise-portrait-version-mismatch expected got)
  (error "define-actor #:version and #:portrait version conflict:"
         'expected: expected 'got: got))

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
    ;; Walk through the body and extract all the keyword arguments which are
    ;; "special" to define-actor
    (define (extract-body-keywords body)
      (let lp ((body body)
               (frozen? #f)
               (version #f)
               (portrait #f)
               (restore #f)
               (upgrade #f)
               (self #f))
        (syntax-case body ()
          ((#:frozen . rest)
           (lp #'rest #t version portrait restore upgrade self))
          ((#:version version . rest)
           (lp #'rest frozen? #'version portrait restore upgrade self))
          ((#:portrait portrait . rest)
           (lp #'rest frozen? version #'portrait restore upgrade self))
          ((#:restore restore . rest)
           (lp #'rest frozen? version portrait #'restore upgrade self))
          ((#:upgrade upgrader . rest)
           (lp #'rest frozen? version portrait restore #'upgrader self))
          ((#:self self . rest)
           (lp #'rest frozen? version portrait restore upgrade #'self))
          (rest-body (values body frozen? version portrait restore upgrade
                             self)))))
    (syntax-case stx ()
      [(_ (constructor-id bcom arg ...) body ...)
       (let ((kwless-body frozen? version portrait restore upgrade self
                          (extract-body-keywords #'(body ...))))
         (with-syntax (((arg-name ...) (args->arg-names #'(arg ...)))
                       ((kwless-body-extra ... kwless-body-final) kwless-body))
           (define constructor
             (with-syntax ((real-constructor
                            #`(lambda* (bcom arg ...)
                               ;; Define the self-portrait in one of several ways depending
                               ;; on whether portrait and/or version are supplied...
                               #,@(cond
                                   ;; If there's a portrait AND a version, we want to enforce
                                   ;; that if the inner portrait gives a portrait that we error
                                   ;; out on seeing another version added
                                   ((and portrait version)
                                    ;; doing the let here makes sure the portrait procedure and
                                    ;; version are instantiated once, not on every call
                                    #`((define self-portrait-proc #,portrait)
                                       (define version #,version)
                                       (define (self-portrait)
                                         (define result (self-portrait-proc))
                                         (if (versioned-data? result)
                                             ;; let's make sure the result's version matches
                                             (if (equal? (versioned-data-version result)
                                                         version)
                                                 ;; the version matches, so just return it
                                                 result
                                                 ;; otherwise else, mismatching versions!
                                                 (raise-portrait-version-mismatch
                                                  version (versioned-data-version result)))
                                             ;; and if it isn't versioned data, let's version it!
                                             (versioned version result)))))
                                   ;; portrait but no version
                                   (portrait
                                    #`((define self-portrait #,portrait)))
                                   ;; version but no portrait
                                   (version
                                    #`((define version #,version)
                                       (define (self-portrait)
                                         (versioned #,version
                                                    (list arg-name ...)))))
                                   ;; default with default version
                                   (else
                                    #'((define (self-portrait)
                                         (list arg-name ...)))))
                               kwless-body-extra ...
                               (portraitize kwless-body-final self-portrait))))
               (if self
                   (let ((constructor-id
                          #`(lambda* (_bcom arg ...)
                             (define constructor-id real-constructor)
                             (define #,self (spawn constructor-id arg ...))
                             #,self)))
                     constructor-id)
                   #`(let ((constructor-id real-constructor))
                       constructor-id))))
           (cond
            ((and frozen? restore)
             ;; Not meaningfully, since it removes the optimization
             ;; Perhaps we could let it go silently, but that seems
             ;; like it might not inform the user right.
             (error "Can't combine #:frozen and #:restore"))
            (frozen?
             #`(define constructor-id
                 #,constructor))
            ;; This *MUST* go above restore.
            (upgrade
             #`(define-redefinable-object-with-rehydrator constructor-id
                 #,constructor
                 (lambda args
                   (define provided-restore #,restore)
                   (define upgrader #,upgrade)
                   (define-values (new-version new-roots)
                     (apply upgrader args))
                   (if provided-restore
                       (apply provided-restore new-version new-roots)
                       (apply spawn constructor-id new-roots)))))

            (restore
             #`(define-redefinable-object-with-rehydrator constructor-id
                 #,constructor
                 #,restore))
            (else
             #`(define-redefinable-object constructor-id
                 #,constructor)))))])))

(define-syntax-rule (define-hackable (constructor-id bcom args ...) body ...)
  (define-redefinable-object constructor-id
    (let ((constructor-id
           (lambda (bcom args ...)
             body ...)))
      constructor-id)))
