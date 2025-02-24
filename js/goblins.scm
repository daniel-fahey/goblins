;;; Copyright 2025 David Thompson <dave@spritely.institute>
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

(use-modules (fibers promises)
             (goblins)
             (hoot ffi))

(define (spawn-vat/js name)
  (spawn-vat #:name name))

(define (call-with-vat/js resolved rejected vat f)
  (call-with-async-result
   resolved rejected
   (lambda ()
     (call-with-vat vat (lambda () (call-external f))))))

(define (wrap-external-function f)
  (lambda args (apply call-external f args)))

(define (spawn/js constructor . args)
  (spawn
   (lambda (bcom)
     (define bcom/js
       (case-lambda
         ((new-behavior)
          (bcom (wrap-external-function new-behavior)))
         ((new-behavior result)
          (bcom (wrap-external-function new-behavior) result))))
     (wrap-external-function
      (apply call-external constructor
             (procedure->external bcom/js)
             args)))))

(define* (on/js vow fulfilled #:optional catch)
  (on vow (wrap-external-function fulfilled)
      #:catch (wrap-external-function catch)))

(values spawn-vat/js
        call-with-vat/js
        spawn/js
        $
        <-
        <-np
        on/js)
