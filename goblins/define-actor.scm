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
  #:use-module ((goblins core) #:select (spawn))
  #:use-module (srfi srfi-11)
  #:use-module ((goblins core-types)
                #:select (portraitize
                          make-redefinable-object
                          redefinable-object?
                          set-redefinable-object-constructor!
                          set-redefinable-object-rehydrator!
                          versioned
                          versioned?
                          versioned-version))
  #:export (define-actor define-hackable))


(define-syntax-rule (define-redefinable-object-with-rehydrator name proc rehydrator)
  (define name
    (cond-expand
     (guile
      (let ((proc* proc)
            (rehydrator* rehydrator))
        (cond
         ((and (defined? 'name) (redefinable-object? name))
          ;; We've already defined this, just update the constructor refr
          (set-redefinable-object-constructor! name proc*)
          (set-redefinable-object-rehydrator! name rehydrator*)
          name)
         (else
          ;; First time (or currently not a redefinable object),
          ;; lets define it.
          (make-redefinable-object proc* rehydrator*)))))
     ;; Hoot programs are currently static objects and so redefinable objects
     ;; are not meaningful for hoot. Consider actors as frozen under hoot.
     (hoot
      (make-redefinable-object proc rehydrator)))))

(define-syntax-rule (define-redefinable-object name proc)
  (define-redefinable-object-with-rehydrator name proc #f))

(define (raise-portrait-version-mismatch expected got)
  (error "define-actor #:version and #:portrait version conflict:"
         'expected: expected 'got: got))

(define-syntax define-actor
  (lambda (stx)
    (define (identifier->keyword id)
      (datum->syntax id (symbol->keyword (syntax->datum id))))
    (define (parse-arg arg)
      (syntax-case arg ()
        (id
         (identifier? #'id)
         #'id)
        ((id default)
         (identifier? #'id)
         #'id)))
    (define (parse-arg-names args)
      ;; Go through each argument to the actor pulling out the
      ;; identifier only (e.g. skip #:key, #:optional, default values,
      ;; etc.).
      (let lp ((args args) (keyword? #f))
        (syntax-case args ()
          (() '())
          ((#:key . rest)
           (lp #'rest #t))
          ((#:allow-other-keys . rest)
           (lp #'rest #f))
          ((#:optional . rest)
           (lp #'rest #f))
          ((#:rest . rest)
           (lp #'rest #f))
          ((arg . rest)
           (let ((id (parse-arg #'arg)))
             ;; If we're handling keyword arguments, add the keyword
             ;; for the identifier as well as the identifier itself so
             ;; that when applied it works as e.g. (#:name name),
             ;; otherwise just add the id.
             (if keyword?
                 (cons* (identifier->keyword id) id (lp #'rest keyword?))
                 (cons id (lp #'rest keyword?))))))))
    ;; Walk through the body and extract all the keyword arguments which are
    ;; "special" to define-actor
    (define (parse-keywords body)
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
          ((kw . _)
           (keyword? (syntax->datum #'kw))
           (syntax-violation 'define-actor "invalid keyword" stx #'kw))
          (_
           (values body frozen? version portrait restore upgrade self)))))
    ;; Parse procedure properties (literal string or vector) from
    ;; body.
    (define (parse-properties body)
      (syntax-case body ()
        (()
         (syntax-violation 'define-actor "empty body" stx))
        ((exp) ; single expression, no properties
         (values #() #'(exp)))
        ((properties body* . body) ; docstring or property list
         (let ((datum (syntax->datum #'properties)))
           (or (string? datum) (vector? datum)))
         (values #'properties #'(body* . body)))
        (_ (values #() body))))
    (define (parse-body body)
      (let*-values (((body frozen? version portrait restore upgrade self)
                     (parse-keywords body))
                    ((properties body)
                     (parse-properties body)))
        (values properties body frozen? version portrait restore upgrade self)))
    (syntax-case stx ()
      ((_ (constructor-id bcom arg ...) body ...)
       (let-values (((properties body frozen? version portrait restore upgrade self)
                     (parse-body #'(body ...))))
         (with-syntax (((arg-name ...) (parse-arg-names #'(arg ...)))
                       ((body ... body*) body))
           (define constructor
             (with-syntax ((real-constructor
                            #`(lambda* (bcom arg ...)
                                ;; Procedure properties will go in the
                                ;; wrapper constructor in the case of
                                ;; #:self.
                                #,(if self #() properties)
                                ;; Define the self-portrait in one of several ways depending
                                ;; on whether portrait and/or version are supplied...
                                #,(cond
                                   ;; If there's a portrait AND a version, we want to enforce
                                   ;; that if the inner portrait gives a portrait that we error
                                   ;; out on seeing another version added
                                   ((and portrait version)
                                    ;; doing the let here makes sure the portrait procedure and
                                    ;; version are instantiated once, not on every call
                                    #`(begin
                                        (define self-portrait-proc #,portrait)
                                        (define version #,version)
                                        (define (self-portrait)
                                          (define result (self-portrait-proc))
                                          (if (versioned? result)
                                              ;; let's make sure the result's version matches
                                              (if (equal? (versioned-version result)
                                                          version)
                                                  ;; the version matches, so just return it
                                                  result
                                                  ;; otherwise else, mismatching versions!
                                                  (raise-portrait-version-mismatch
                                                   version (versioned-version result)))
                                              ;; and if it isn't versioned data, let's version it!
                                              (versioned version result)))))
                                   ;; portrait but no version
                                   (portrait
                                    #`(define self-portrait #,portrait))
                                   ;; version but no portrait
                                   (version
                                    #`(begin
                                        (define version #,version)
                                        (define (self-portrait)
                                          (versioned #,version
                                                     (list arg-name ...)))))
                                   ;; default with default version
                                   (else
                                    #'(define (self-portrait)
                                        (list arg-name ...))))
                                body ...
                                (portraitize body* self-portrait))))
               (if self
                   (let ((constructor-id
                          #`(lambda* (_bcom arg ...)
                              #,properties
                              (define constructor-id real-constructor)
                              (define #,self (spawn constructor-id arg-name ...))
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
                   (define refr
                     (if provided-restore
                         (apply provided-restore new-version new-roots)
                         (apply spawn constructor-id new-roots)))
                   (if (versioned? refr)
                       refr
                       (versioned new-version refr)))))

            (restore
             #`(define-redefinable-object-with-rehydrator constructor-id
                 #,constructor
                 #,restore))
            (else
             #`(define-redefinable-object constructor-id
                 #,constructor)))))))))

(define-syntax-rule (define-hackable (constructor-id bcom args ...) body ...)
  (define-redefinable-object constructor-id
    (let ((constructor-id
           (lambda (bcom args ...)
             body ...)))
      constructor-id)))
