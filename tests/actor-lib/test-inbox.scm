;;; Copyright 2024 Jessica Tallon
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

(define-module (tests actor-lib test-inbox)
  #:use-module (goblins)
  #:use-module (goblins actor-lib inbox)
  #:use-module (tests utils)
  #:use-module (srfi srfi-64))

(test-begin "test-inbox")

(define am (make-actormap))

(define-values (read write stop!)
  (actormap-run! am spawn-inbox))

;; Can write a message and then receive it
(actormap-poke! am write 'foo)
(test-equal "Message written to inbox can be read"
  'foo
  (actormap-poke! am read))

;; Can write multiple messages and read them in order
(actormap-poke! am write 'one)
(actormap-poke! am write 'two)
(actormap-poke! am write 'three)
(test-equal "Message written to inbox are read in correct order"
  '(one two three)
  (list (actormap-poke! am read)
        (actormap-poke! am read)
        (actormap-poke! am read)))

;; Reading with no messages gives a vow
(define vow (actormap-peek am read))
(test-assert "Got a vow when message queue is empty"
  (actormap-run am (lambda _ promise-refr? vow)))

(test-equal "When message is written, the promise is fulfilled"
  'four
  (actormap-run!
   am
   (lambda ()
     (define vow ($ read))
     ($ write 'four)
     ($ vow))))

;; check messages are written in the correct order to promises handed out
(define vow1 (actormap-poke! am read))
(define vow2 (actormap-poke! am read))
(actormap-poke! am write 'five)
(actormap-poke! am write 'six)

(test-equal "Messages are fulfilled to promises in correct order"
  '(five six)
  (actormap-run am (lambda () (list ($ vow1) ($ vow2)))))

(test-equal "When inbox is stopped, it breaks pending read vows"
  '(inbox-closed #f)
  (let ((err #f)
        (success #f))
    (actormap-churn-run!
     am
     (lambda ()
       (define vow ($ read))
       ($ stop!)
       (on vow
           (lambda (val)
             (set! success val))
           #:catch
           (lambda (got-err)
             (set! err got-err)))))
    (list err success)))

(test-assert "Cannot read from inbox when stopped"
  (lambda () (actormap-poke! am read)))
(test-assert "Cannot write to inbox when stopped"
  (lambda () (actormap-poke! am write 'foo)))

;; Test persistence, should restore messages too
(define-values (read write stop!)
  (actormap-run! am spawn-inbox))

;; Write some messages
(actormap-poke! am write 'one)
(actormap-poke! am write 'two)
(actormap-poke! am write 'three)

;; Save and restore
(define-values (am* read* write* stop!*)
  (persist-and-restore am inbox-env read write stop!))

(test-equal "Messages persist after storing"
  '(one two three)
  (list (actormap-poke! am* read*)
        (actormap-poke! am* read*)
        (actormap-poke! am* read*)))

(actormap-poke! am stop!)
(define-values (am* read* write* stop!*)
  (persist-and-restore am inbox-env read write stop!))

(test-error "Stopped inbox state persists after restoring"
            #t
            (actormap-poke! am write 'foo))

(test-end "test-inbox")
