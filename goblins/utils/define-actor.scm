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
  #:use-module (goblins core)
  #:export (define-actor))

(define-syntax-rule (define-actor (constructor-id bcom args ...) body ...)
  (define (constructor-id bcom args ...)
    (define main-beh
      body ...)
    (define (self-portrait)
      (list args ...))
    (portraitize main-beh self-portrait)))
