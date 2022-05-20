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

(define-module (goblins default-vat-scheduler)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 threads)
  #:export (default-vat-scheduler))

;; Kludge to get around change of interface for accessing current
;; fiber between Fibers 1.0.0 and 1.1.0
(define %current-scheduler
  (catch
    #t
    ;; Fibers 1.0.0
    (lambda ()
      (let ((current-fiber (@@ (fibers internal) current-fiber))
            (fiber-scheduler (@@ (fibers internal) fiber-scheduler)))
        (lambda ()
          (fiber-scheduler (current-fiber)))))
    ;; Fibers 1.1.0
    (lambda _
      (@@ (fibers scheduler) current-scheduler))))

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
                  (%current-scheduler))
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
          (get-message result-ch)))))

