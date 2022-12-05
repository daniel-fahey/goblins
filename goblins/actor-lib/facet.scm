;;; Copyright 2019-2022 Christine Lemmer-Webber
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
  #:use-module (ice-9 match)
  #:export (^facet facet))

(define* (^facet bcom wrap-me
                 #:key [sync? #f]
                 #:rest methods)
  (define $/<- (if sync? $ <-))
  (lambda args
    (match args
      [((? symbol? method) args ...)
       (unless (member method methods)
         (error (format #f "Access to method ~a denied" method)))
       (apply $/<- wrap-me method args)]
      [_ "Requires symbol-based method dispatch"])))

(define* (facet wrap-me
                #:key [sync? #f]
                #:rest methods)
  (apply spawn-named (procedure-name wrap-me) ^facet
         #:sync? sync? methods))
