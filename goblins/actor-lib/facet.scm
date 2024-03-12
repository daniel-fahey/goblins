;;; Copyright 2019-2022 Christine Lemmer-Webber
;;; Copyright 2023 Juliana Sims
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

(define-module (goblins actor-lib facet)
  #:use-module (goblins)
  #:use-module (goblins actor-lib opportunistic)
  #:use-module (ice-9 match)
  #:export (^facet facet facet-env))

;; TODO: When define-actor supports #:rest, use define-actor instead.
(define* (^facet bcom wrap-me #:rest methods)
  "Construct an object which limits user access to methods of WRAP-ME.

The METHODS argument is the collection of methods of WRAP-ME to be
exposed to the user.

The resulting actor can be invoke with any of METHODS."
  (define $/<- (select-$/<- wrap-me))
  (define main-beh
    (lambda args
      (match args
	[((? symbol? method) args ...)
	 (unless (member method methods)
           (error (format #f "Access to method ~a denied" method)))
	 (apply $/<- wrap-me method args)]
	[_ "Requires symbol-based method dispatch"])))
  (define (self-portrait)
    (cons wrap-me methods))
  (portraitize main-beh self-portrait))

(define* (facet wrap-me #:rest methods)
  "Return an object which limits user access to methods of WRAP-ME.

The METHODS argument is the collection of methods of WRAP-ME to be
exposed to the user.

Type: Actor (Optional (#:async? Boolean)) (Symbol ...) -> Actor"
  (apply spawn-named (procedure-name wrap-me) ^facet
         methods))

(define facet-env
  (make-persistence-env
   `((((goblins actor-lib facet) ^facet) ,^facet))))
