;;; Copyright 2021 Christine Lemmer-Webber
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

(define-module (goblins)
  #:use-module (guile)
  #:use-module ((goblins core) #:select (spawn) #:prefix core:)
  #:use-module ((goblins core) #:hide (spawn))
  #:use-module (goblins define-actor)
  #:use-module (goblins migrations)
  #:use-module (goblins repl)
  #:use-module (goblins vat)
  #:re-export (live-refr?
               local-refr?
               remote-refr?
               promise-refr?
               local-object-refr?
               local-promise-refr?
               remote-object-refr?
               remote-promise-refr?

               near-refr?
               far-refr?

               make-actormap
               make-transactormap
               make-whactormap

               actormap-spawn
               actormap-spawn!
               ;; actormap-spawn-mactor!

               actormap-turn*
               actormap-turn

               actormap-turn-message

               actormap-peek
               actormap-poke!
               actormap-reckless-poke!

               actormap-run
               actormap-run!
               actormap-run*

               actormap-churn
               actormap-churn-run
               actormap-churn-run!

               dispatch-message
               dispatch-messages

               whactormap?
               transactormap-merge!
               transactormap-buffer-merge!

               spawn-named
               $
               <-np <-
               on
               on-sever

               <-np-extern
               listen-to

               await await*
               <<-

               spawn-promise-and-resolver

               make-persistence-env
               namespace-env
               persistence-env-compose
               portraitize
               actormap-take-portrait
               actormap-replace-behavior
               actormap-replace-behavior!
               actormap-restore!
               actormap-restore-from-store!
               actormap-save-to-store!

               versioned

               make-vat
               vat?
               vat-name
               vat-running?
               vat-halt!
               vat-start!
               call-with-vat
               with-vat
               spawn-vat
               spawn-persistent-vat
               vat-replace-behavior!
               define-vat-run
               vat-take-portrait!
               ^persistence-registry

               define-actor
               migrations

               ;; Deprecated
               spawn-promise-cons
               spawn-promise-values)
  ;; Re-exporting and replacing isn't possible, so we must hack.
  #:export (spawn)
  #:replace (spawn))

(define spawn core:spawn)
