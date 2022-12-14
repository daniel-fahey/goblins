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

(define-module (goblins actor-lib pubsub)
  #:use-module (goblins core)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib methods)
  #:export (^pubsub))

(define (^pubsub bcom . initial-subscribers)
  (define subscribers
    (apply spawn ^seteq initial-subscribers))

  (define (publish . args)
    (map
     (lambda (subscriber)
       (apply <-np subscriber args))
     ($ subscribers 'as-list))
    #t)

  (methods
   ((subscribe subscriber) ($ subscribers 'add subscriber))
   ((unsubscribe subscriber) ($ subscribers 'remove subscriber))
   (publish publish)
   ((subscribers) ($ subscribers 'as-list))))
