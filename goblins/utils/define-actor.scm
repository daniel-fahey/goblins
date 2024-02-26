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
(define-module (goblins utils define-actor)
  #:use-module ((goblins core-types)
                #:select (portraitize
                          make-redefinable-object
                          set!-redefinable-object-constructor))
  #:export (define-actor define-hackable))

(define-syntax-rule (define-actor (constructor-id bcom args ...) body ...)
  (if (module-defined? (current-module) 'constructor-id)
      ;; We've already defined this, just update the constructor refr
      (set!-redefinable-object-constructor
       constructor-id
       (let ((constructor-id
              (lambda* (bcom args ...)
                (define main-beh
                  body ...)
                (define (self-portrait)
                  (list args ...))
                (portraitize main-beh self-portrait))))
         constructor-id))
      ;; First time, lets define it.
      (module-define!
       (current-module)
       'constructor-id
       (make-redefinable-object
        (let ((constructor-id
               (lambda* (bcom args ...)
                 (define main-beh
                   body ...)
                 (define (self-portrait)
                   (list args ...))
                 (portraitize main-beh self-portrait))))
          constructor-id)))))

(define-syntax-rule (define-hackable (constructor-id bcom args ...) body ...)
  (if (module-defined? (current-module) 'constructor-id)
      ;; We've already defined this, just update the constructor refr
      (set!-redefinable-object-constructor
       constructor-id
       (let ((constructor-id (lambda* (bcom args ...)
                               body ...)))
         constructor-id))
      ;; First time, lets define it.
      (module-define!
       (current-module)
       'constructor-id
       (make-redefinable-object
        (let ((constructor-id
               (lambda* (bcom args ...)
                 body ...)))
          constructor-id)))))
