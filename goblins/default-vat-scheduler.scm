;;; Copyright 2022 Christine Lemmer-Webber
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

;;; The reason for this whole module is to try to prevent creating
;;; so many threads that epoll gets mad.  Having a shared scheduler
;;; mitigates this.

(define-library (goblins default-vat-scheduler)
  (export default-vat-scheduler)
  (cond-expand
   (hoot
    (import (only (scheme base) define))
    (begin (define (default-vat-scheduler) #f)))
   (guile
    (import (guile)
            (fibers)
            (fibers channels)
            (fibers conditions)
            (only (fibers scheduler)
                  current-scheduler)
            (ice-9 atomic)
            (ice-9 threads))
    (begin
      ;; A shared Fibers scheduler to default most vats connecting to.
      ;; We might prefer eventually to delay booting this up as long
      ;; as possible.
      (define %vat-sched
        (make-atomic-box #f))
      (define (default-vat-scheduler)
        "Returns the current default vat scheduler, or makes one if
appropriate"
        (or (atomic-box-ref %vat-sched)   ; already have a scheduler
            (begin                        ; or, try to make a new one
              (let ((result-ch (make-channel)))
                (make-thread
                 (lambda ()
                   (run-fibers
                    (lambda ()
                      ;; attempt to install this as the current scheduler
                      (define this-sched
                        (current-scheduler))
                      (define prev-sched
                        (atomic-box-compare-and-swap! %vat-sched #f this-sched))
                      (if prev-sched
                          ;; someone else installed a scheduler in-between
                          (put-message result-ch prev-sched)
                          ;; our scheduler is the new one
                          (begin
                            (put-message result-ch this-sched)
                            ;; since we're the new scheduler, run forever...
                            (wait (make-condition))))))))
                (get-message result-ch)))))))))
