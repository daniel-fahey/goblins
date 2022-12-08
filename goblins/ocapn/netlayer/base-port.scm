;;; Copyright 2021-2022 Christine Lemmer-Webber
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

(define-module (goblins ocapn netlayer base-port)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 iconv)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins inbox)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer utils)
  #:use-module (goblins contrib syrup)
  #:export (^base-port-netlayer))

(define (^base-port-netlayer bcom our-location
                             incoming-accept
                             outgoing-connect-location)
  "A basis for defining netlayers."
  (define our-netlayer-name
    (ocapn-machine-transport our-location))

  ;; (define shutdown-time (make-condition))
  (define (start-listen-thread conn-establisher)
    (define (listen)
      (define incoming-port (incoming-accept))
      (define-values (read-message write-message)
        (read-write-procs incoming-port incoming-port))
      (<-np-extern conn-establisher read-message write-message #t)
      (listen))
    ;; Simplified while we're trying to get this to work.
    ;; But the dynamic-wind hack above won't work anyway because,
    ;; well, fibers normally suspends/resumes all the time and
    ;; this would get triggered incorrectly.  We need new, smarter
    ;; code for how to shut this down.
    (syscaller-free-fiber listen))

  (define base-beh
    (methods
     [(netlayer-name) our-netlayer-name]
     [(our-location) our-location]))

  ;; State of the netlayer before it gets called with 'setup
  (define pre-setup-beh
    (extend-methods
     base-beh
     ;; The machine is now wiring us up with the appropriate behavior for
     ;; when a new connection comes in
     [(setup conn-establisher)
      (start-listen-thread conn-establisher)
      ;; Now that we're set up, transition to the main behavior
      (bcom (ready-beh conn-establisher))]))
  (define (ready-beh conn-establisher)
    (extend-methods
     base-beh
     [(self-location? loc)
      (same-machine-location? our-location loc)]
     [(connect-to remote-machine)
      (unless (eq? (ocapn-machine-transport remote-machine)
                   our-netlayer-name)
        (error "Mismatched netlayer:"
               (ocapn-machine-transport remote-machine)
               our-netlayer-name))
      ;; Asynchronously set up connection.  Once it's ready, we'll
      ;; return the value from the connection establisher
      ;; (which itself returns the meta-bootstrap-vow)
      (define read-write-message-vow
        (spawn-fibrous-vow
         (lambda ()
           (define connected-port
             (outgoing-connect-location remote-machine))
           (define-values (read-message write-message)
             (read-write-procs connected-port connected-port))
           (list read-message write-message))))

      (on read-write-message-vow
          (match-lambda
            ((read-message write-message)
             (<- conn-establisher read-message write-message #f)))
          #:promise? #t)]))
  pre-setup-beh)
