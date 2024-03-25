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

(define-module (tests actor-lib test-ward)
  #:use-module (goblins core)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins define-actor)
  #:use-module (goblins actor-lib ward)
  #:use-module (goblins actor-lib methods)
  #:use-module (tests utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(test-begin "test-ward")

(define-actor (^inbox bcom mailbox-name admin-warden
		      #:optional [messages '()]
		      #:key revoked?)
  (define admin-methods
    (methods
     [(revoke) (bcom (^inbox bcom mailbox-name admin-warden
			     #:revoked? #t))]
     [(get-messages) messages]
     [(set-name new-name #:key [upcase? #f])
      (bcom (^inbox bcom (if upcase?
                             (string-upcase new-name)
                             new-name)
		    admin-warden
                    messages))]))
  (define public-methods
    (methods
     [(send-message msg)
      (bcom (^inbox bcom mailbox-name
		    admin-warden (cons msg messages)))]
     [(mailbox-name) mailbox-name]))
  
  (define revoked-beh
    (lambda _ (error "revoked")))

  (if revoked?
      revoked-beh
      (ward admin-warden admin-methods
            #:extends public-methods)))

(define am (make-actormap))

(define-values (inbox admin-incanter)
  (actormap-run!
   am
   (lambda ()
     (define-values (admin-warden admin-incanter)
       (spawn-warding-pair))
     (values (spawn ^inbox "My First Inbox" admin-warden)
             admin-incanter))))

(test-equal
 "My First Inbox"
 (actormap-peek am inbox 'mailbox-name))

(test-error
 "Can't just set the name without incanter"
 #t
 (actormap-poke! am inbox 'set-name "A brand new name"))

(test-assert
 "Can set the name through the incanter without erroring out"
 (begin
   (actormap-poke! am admin-incanter
                   inbox 'set-name "New name"
                   #:upcase? #t)
   #t))

(test-equal "New name successfully set via incanter"
 "NEW NAME"
 (actormap-peek am inbox 'mailbox-name))

(define some-other-incanter
  (actormap-run! am
                 (lambda ()
                   (define-values (_some-warden some-incanter)
                     (spawn-warding-pair))
                   some-incanter)))

(test-error
 "Can't just set the name without incanter"
 #t
 (actormap-poke! am some-other-incanter
                 inbox 'set-name "A brand new name"))

(define inbox-admin
  (actormap-run! am
                 (lambda () (enchant admin-incanter inbox))))

(test-assert
 "Can set the name through the incantified proxy without erroring out"
 (begin
   (actormap-poke! am inbox-admin
                   'set-name "Another new name"
                   #:upcase? #t)
   #t))

(test-equal "New name successfully set via incantified proxy"
 "ANOTHER NEW NAME"
 (actormap-peek am inbox 'mailbox-name))

;; ;; allow for warding multiple things at once
;; (define (^multitool bcom tool1-warden tool2-warden)
;;   (ward tool1-warden (lambda _ 'tool1)
;;         tool2-warden (lambda _ 'tool2)
;;         #:extends (lambda _ 'fallback)))
;; (define-values (tool1-warden tool1-incanter)
;;   (am-run (spawn-warding-pair)))
;; (define-values (tool2-warden tool2-incanter)
;;   (am-run (spawn-warding-pair)))

;; (define mtool
;;   (am-run (spawn ^multitool tool1-warden tool2-warden)))

;; (test-equal?
;;  "multi-ward test 1"
;;  (am-run ($ tool1-incanter mtool))
;;  'tool1)
;; (test-equal?
;;  "multi-ward test 2"
;;  (am-run ($ tool2-incanter mtool))
;;  'tool2)
;; (test-equal?
;;  "multi-ward fallback"
;;  (am-run ($ mtool))
;;  'fallback)

;; Ensure we've prevented against replay attacks
(define-values (mitm-warden mitm-incanter)
  (actormap-run! am (lambda () (spawn-warding-pair))))

(define (^simply-warded bcom)
  (ward mitm-warden (lambda _ 'warded)
        #:extends (lambda _ 'extended)))

(define simply-warded
  (actormap-spawn! am ^simply-warded))

(define caught-args #f)
(define (^mitm _bcom wraps)
  (lambda args
    (set! caught-args args)
    (apply $ wraps args)))
(define mitm (actormap-spawn! am ^mitm simply-warded))

(test-assert
 "Man in the middle is able to run once..."
 (begin
   (actormap-poke! am mitm-incanter mitm)
   #t))

(test-assert
 "...and we were able to catch the arguments..."
 caught-args)

(test-error
 "...but we can't replay with the caught sealed arguments"
 #t
 (actormap-run! am (lambda () (apply $ mtool caught-args))))

;; Persistence tests
(actormap-poke! am admin-incanter inbox 'set-name "My first name")
(define inbox-env
  (make-persistence-env
   `((((tests actor-lib test-ward) ^inbox) ,^inbox))
   #:extends ward-env))
(define-values (am* inbox* admin-incanter*)
  (persist-and-restore am inbox-env inbox admin-incanter))

(test-equal "Check warding extends work after resturation"
 "My first name"
 (actormap-peek am* inbox* 'mailbox-name))

(test-error
 "Test we can't set the name without an incanter after resturation"
 #t
 (actormap-poke! am* inbox* 'set-name "A brand new name"))

(actormap-poke! am* admin-incanter* inbox* 'set-name "My brand new restored name")
(test-equal "Check name is set on restored inbox through warding"
 "My brand new restored name"
 (actormap-peek am* inbox* 'mailbox-name))


(test-end "test-ward")
