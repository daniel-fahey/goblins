;;; Copyright 2024 Juliana Sims
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

(define-library (goblins ocapn gc)
  (export make-captp-gc
          captp-gc?
          captp-gc-register!
          captp-gc-halt!
          captp-gc-get)
  (import (fibers)
          (fibers channels)
          (fibers conditions)
          (goblins core-types)
          (goblins ocapn captp-types)
          (except (guile) sleep)
          (ice-9 match)
          (srfi srfi-9))
  (cond-expand
   (hoot (import (hoot finalization)))
   (guile (import (fibers operations)
                  (fibers timers))))
  (begin
    (define-record-type <captp-gc>
      (%make-captp-gc wrapped channel stopped?)
      captp-gc?
      (wrapped unwrap-captp-gc set-captp-gc-wrapped!)
      (channel captp-gc-ch)
      (stopped? captp-gc-stopped? set-captp-gc-stopped?!))
    (define (captp-gc-halt! gc)
      (cond-expand
       (hoot (set-captp-gc-stopped?! gc #t))
       (guile (signal-condition! (captp-gc-stopped? gc)))))
    (define (captp-gc-get gc) (get-message (captp-gc-ch gc)))
    (define (make-captp-gc ch)
      (cond-expand
       (hoot
        (let ((gc (%make-captp-gc #f ch #f)))
          (set-captp-gc-wrapped!
           gc
           (make-finalization-registry
            (lambda (handle)
              (unless (captp-gc-stopped? gc)
                (put-message (captp-gc-ch gc) handle)))))
          gc))
       (guile
        (let ((guardian (make-guardian))
              (stopped? (make-condition)))
          (define (check-guardian)
            (when (perform-operation
                   (choice-operation
                    (wrap-operation (wait-operation stopped?)
                                    (lambda () #f))
                    (wrap-operation (sleep-operation 1)
                                    (lambda () #t))))
              (let lp ()
                (match (guardian)
                  ;; guardian is empty
                  (#f (check-guardian))
                  ;; gc a question
                  ((? question-finder? question)
                   (put-message ch `(gc-question
                                     ,(question-finder-sealed-pos question)))
                   (lp))
                  ;; gc an import
                  ((? remote-refr? import)
                   (put-message ch `(gc-remote-refr
                                     ,(remote-refr-sealed-pos import)))
                   (lp))))))
          (spawn-fiber check-guardian)
          (%make-captp-gc guardian ch stopped?)))))
    (define (captp-gc-register! gc obj)
      (cond-expand
       (hoot
        (finalization-registry-register!
         (unwrap-captp-gc gc) obj
         (match obj
           ((? question-finder? question)
            `(question-finder
              ,(question-finder-sealed-pos question)))
           ((? remote-refr? import)
            `(remote-refr
              ,(remote-refr-sealed-pos import))))))
       (guile
        ((unwrap-captp-gc gc) obj))))))
