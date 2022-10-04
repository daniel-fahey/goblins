;;; Copyright 2022 Jessica Tallon
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

(define-module (tests utils)
  #:use-module (goblins)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:export (resolve-vow-and-return-result))

(define (resolve-vow-and-return-result vat goblins-thunk)
  (run-fibers
   (lambda ()
     (define vow (vat goblins-thunk))
     (define results-ch (make-channel))
     (vat
      (lambda ()
        (on vow
            (lambda args
              (spawn-fiber
               (lambda ()
                 (put-message results-ch (apply vector 'ok args))))
              'ok)
            #:catch
            (lambda err
              (spawn-fiber
               (lambda ()
                 (put-message results-ch (vector 'err err))))
              'err))))
     (get-message results-ch))))
