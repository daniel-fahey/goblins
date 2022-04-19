;;; Copyright 2020-2022 Christine Lemmer-Webber
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

(define-module (tests actor-lib test-ticker)
  #:use-module (goblins core)
  #:use-module (goblins actor-lib ticker)
  #:use-module (goblins actor-lib cell)
  #:use-module (srfi srfi-64))

(test-begin "test-ticker")

(define am (make-actormap))

(define ticker (actormap-run! am spawn-ticker))

(define joe-speaks-here
  (actormap-spawn! am ^cell))
(define jane-speaks-here
  (actormap-spawn! am ^cell))
(define* (^malaise-sufferer bcom ticky name speaking-cell
                            #:optional [maximum-suffering 3])
  (let loop ((n 1))
    (lambda ()
      (if (> n maximum-suffering)
          (begin
            ($ speaking-cell
               (format #f "<~a> you know what? I'm done."
                       name))
            ($ ticky 'die))
          (begin
            ($ speaking-cell
               (format #f "<~a> sigh number ~a"
                       name n))
            (bcom (loop (1+ n))))))))
(define joe
  (actormap-poke! am ticker 'to-tick
                  (lambda (ticky)
                    (spawn ^malaise-sufferer ticky "joe"
                           joe-speaks-here))))
(define jane
  (actormap-poke! am ticker 'to-tick
                  (lambda (ticky)
                    (spawn ^malaise-sufferer ticky "jane"
                           jane-speaks-here
                           2))))
(actormap-poke! am ticker 'tick)
(test-equal
 (actormap-peek am joe-speaks-here)
 "<joe> sigh number 1")
(test-equal
 (actormap-peek am jane-speaks-here)
 "<jane> sigh number 1")

(actormap-poke! am ticker 'tick)
(test-equal
 (actormap-peek am joe-speaks-here)
 "<joe> sigh number 2")
(test-equal
 (actormap-peek am jane-speaks-here)
 "<jane> sigh number 2")

(actormap-poke! am ticker 'tick)
(test-equal
 (actormap-peek am joe-speaks-here)
 "<joe> sigh number 3")
(test-equal
 (actormap-peek am jane-speaks-here)
 "<jane> you know what? I'm done.")

(actormap-poke! am ticker 'tick)
(test-equal
 (actormap-peek am joe-speaks-here)
 "<joe> you know what? I'm done.")
(test-equal
 (actormap-peek am jane-speaks-here)
 "<jane> you know what? I'm done.")

(test-end "test-ticker")
