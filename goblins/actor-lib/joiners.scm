;;; Copyright 2022-2025 Jessica Tallon
;;; Copyright 2023 Juliana Sims
;;; Copyright 2024 David Thompson <dave@spritely.institute>
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

(define-module (goblins actor-lib joiners)
  #:use-module (goblins)
  #:use-module (goblins actor-lib cell)
  #:export (all-of all-of* race race*))

(define (all-of* promises)
  "Return a promise which resolves on resolution of all PROMISES.

Type: (Listof Promise) -> Promise"
  (define-cell waiting
    promises)
  (define-cell results
    '())
  (define-cell broken?
    #f)

  (define-values (join-promise join-resolver)
    (spawn-promise-and-resolver))

  (define (results->list results-alist)
    (map
     (lambda (promise)
       (cdr (assq promise results-alist)))
     promises))

  (define (resolve-promise promise)
    (on promise
        (lambda (result)
          (unless ($ broken?)
            (let ((new-results (acons promise result ($ results)))
                  (new-waiting (delq promise ($ waiting))))
              (if (null? new-waiting)
                  (<-np join-resolver 'fulfill
                        (results->list new-results))
                  (begin ($ waiting new-waiting)
                         ($ results new-results))))))
        #:catch
        (lambda (err)
          (unless ($ broken?)
            ($ broken? #t)
            (<-np join-resolver 'break err)))))

  (if (null? promises)
      (<-np join-resolver 'fulfill '())
      (for-each resolve-promise promises))

  join-promise)

(define (all-of . promises)
  "Return a promise which resolves on resolution of all PROMISES.

Type: Promise ... -> Promise"
  (all-of* promises))

(define (race . promises)
  "Return a promise which resolves when the first promise in @var{promises}
settles.  If that promise is broken then so is the returned promise.

Type: Promise ... -> Promise"
  (race* promises))

(define (race* promises)
  "Return a promise which resolves when the first promise in @var{promises}
settles.  If that promise is broken then so is the returned promise.

Type: (ListOf Promise) -> Promise"
  (define-values (race-promise race-resolver)
    (spawn-promise-and-resolver))
  (for-each (lambda (promise)
              (on promise
                  (lambda (result)
                    (<-np race-resolver 'fulfill result))
                  #:catch
                  (lambda (err)
                    (<-np race-resolver 'break err))))
            promises)
  race-promise)
