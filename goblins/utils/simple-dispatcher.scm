;;; Copyright 2021 Christine Lemmer-Webber
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

(define-module (goblins utils simple-dispatcher)
  #:export (define-simple-dispatcher))

(define-syntax-rule (define-simple-dispatcher id [method-name method-handler] ...)
  (define id
    (let* ((all-methods (list (cons 'method-name method-handler) ...))
           (these-methods
            (lambda (method . args)
              (let ((found-method (assq-ref all-methods method)))
                (if found-method
                    (apply found-method args)
                    (error 'connector-dispatcher-error
                           "unknown method: ~a" method))))))
      (set-procedure-property! these-methods 'name 'id)
      these-methods)))
