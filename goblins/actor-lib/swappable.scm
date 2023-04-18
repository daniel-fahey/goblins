;;; Copyright 2020-2021 Christine Lemmer-Webber
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

(define-module (goblins actor-lib swappable)
  #:use-module (goblins core)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins utils assert-type)
  #:export (swappable))

(define* (swappable initial-target
                    #:optional
                    [proxy-name
                     (string->symbol
                      (format #f "swappable: ~a"
                              (local-object-refr-debug-name initial-target)))])
  "Return a proxy providing access to INITIAL-TARGET and a swap
capability, accepting a single argument of an actor to switch out with
INITIAL-TARGET. PROXY-NAME, if provided, is the debug name for the proxy.

Type: Actor [String] -> (Values Actor Actor)"
  (define-cell target
    initial-target)

  (define (^proxy _bcom)
    (lambda args
      (apply $ ($ target) args)))

  (define proxy
    (spawn-named proxy-name ^proxy))

  (define (swap new-target)
    (assert-type new-target local-refr?)
    ($ target new-target))

  (values proxy swap))
