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
  #:use-module (goblins actor-lib define-actor)
  #:use-module (goblins actor-lib ticker)
  #:use-module (goblins actor-lib cell)
  #:use-module (tests utils)
  #:use-module (srfi srfi-64))

(test-begin "test-ticker")

(define am (make-actormap))

(define ticker (actormap-run! am spawn-ticker))

(define joe-speaks-here
  (actormap-spawn! am ^cell))
(define jane-speaks-here
  (actormap-spawn! am ^cell))
(define-actor (^malaise-sufferer _bcom ticky name speaking-cell
				 #:optional
				 [maximum-suffering 3]
				 [init-n 1])
  (define n-cell (spawn ^cell init-n))
  (lambda ()
    (let ((n ($ n-cell)))
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
            ($ n-cell (+ n 1)))))))
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
 "<joe> sigh number 1"
 (actormap-peek am joe-speaks-here))
(test-equal
 "<jane> sigh number 1"
 (actormap-peek am jane-speaks-here))

(actormap-poke! am ticker 'tick)
(test-equal
 "<joe> sigh number 2"
 (actormap-peek am joe-speaks-here))
(test-equal
 "<jane> sigh number 2"
 (actormap-peek am jane-speaks-here))

(actormap-poke! am ticker 'tick)
(test-equal
 "<joe> sigh number 3"
 (actormap-peek am joe-speaks-here))
(test-equal
 "<jane> you know what? I'm done."
 (actormap-peek am jane-speaks-here))

(actormap-poke! am ticker 'tick)
(test-equal
 "<joe> you know what? I'm done."
 (actormap-peek am joe-speaks-here))
(test-equal
 "<jane> you know what? I'm done."
 (actormap-peek am jane-speaks-here))

;; Persistence
(define ticker (actormap-run! am spawn-ticker))
(actormap-poke! am ticker 'to-tick
                (lambda (ticky)
                  (spawn ^malaise-sufferer ticky "joe"
                         joe-speaks-here
			 2)))
(actormap-poke! am ticker 'to-tick
		(lambda (ticky)
		  (spawn ^malaise-sufferer ticky "jane"
			 jane-speaks-here
                         1)))
(actormap-churn-run! am (lambda () ($ ticker 'tick)))
(define env
  (make-persistence-env
   `((((tests actor-lib test-ticker) ^malaise-sufferer) ,^malaise-sufferer))
   #:extends (list cell-env ticker-env)))
(define-values (am* ticker* joe-speaks-here* jane-speaks-here*)
  (persist-and-restore am env ticker joe-speaks-here jane-speaks-here))

(test-equal
 "<joe> sigh number 1"
 (actormap-peek am joe-speaks-here))
(test-equal
 "<jane> sigh number 1"
 (actormap-peek am jane-speaks-here))

(actormap-poke! am ticker 'tick)

(test-equal
 "<joe> sigh number 2"
 (actormap-peek am joe-speaks-here))
(test-equal
 "<jane> you know what? I'm done."
 (actormap-peek am jane-speaks-here))

(test-end "test-ticker")
