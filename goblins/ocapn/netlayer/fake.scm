;;; Copyright 2021-2022 Christine Lemmer-Webber
;;; Copyright 2022-2024 Jessica Tallon
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

(define-module (goblins ocapn netlayer fake)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module ((goblins core) #:renamer (lambda (x) (if (eq? x '$) '$C x)))
  #:use-module (goblins vat)
  #:use-module (goblins inbox)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib io)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins contrib syrup)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:export (^fake-network ^fake-netlayer))

(define (^fake-network _bcom)
  (define routes (spawn ^ghash))
  (methods
   [(register name new-conn-ch)
    ($C routes 'set name new-conn-ch)]
   [(connect-to name)
    (define connection-ch
      ($C routes 'ref name))
    (when (not (channel? connection-ch))
      (error (format #t "No connection found by name: ~a" name)))
    (define-values (me-enq-ch me-deq-ch me-stop?)
      (spawn-delivery-agent))
    (define-values (them-enq-ch them-deq-ch them-stop?)
      (spawn-delivery-agent))
    (syscaller-free-fiber
     (lambda ()
       (put-message connection-ch (list '*incoming-new-conn* me-enq-ch them-deq-ch))))
    (list '*outgoing-new-conn* me-deq-ch them-enq-ch)]))

(define (^message-io _bcom incoming-ch outgoing-ch)
  (define incoming-io (spawn ^io incoming-ch))
  (define outgoing-io (spawn ^io outgoing-ch))
  
  (methods
   [(read-message unmarshallers)
    (<- incoming-io
        (lambda (ch)
          (define msg (get-message ch))
          (syrup-decode msg #:unmarshallers unmarshallers)))]
   [(write-message msg marshallers)
    (<-np outgoing-io
        (lambda (ch)
          (put-message ch
                       (syrup-encode msg #:marshallers marshallers))
          *unspecified*))]))

(define (^fake-netlayer _bcom our-name network new-conn-ch)
  (define our-location (make-ocapn-node 'fake our-name #f))
  (define new-connection-io (spawn ^io new-conn-ch))
  (define (start-listening conn-establisher)
    (on (<- new-connection-io get-message)
        (match-lambda
          (('*incoming-new-conn* them-enq-ch me-deq-ch)
           (<-np-extern conn-establisher
                        (spawn ^message-io me-deq-ch them-enq-ch)
                        #f)))
        #:finally
        (lambda ()
          (start-listening conn-establisher))))

  (define (^netlayer bcom)
    (define base-beh
      (methods
       [(netlayer-name) 'fake]
       [(our-location) our-location]))

    (define pre-setup-beh
      (extend-methods
       base-beh
       [(setup conn-establisher)
        (start-listening conn-establisher)
        (bcom (ready-beh conn-establisher))]))

    (define (ready-beh conn-establisher)
      (extend-methods
       base-beh
       [(self-location? loc)
        (same-node-location? our-location loc)]
       [(connect-to remote-node)
        (match remote-node
          (($ <ocapn-node> 'fake name #f)
           (on (<- network 'connect-to name)
               (match-lambda
                 (('*outgoing-new-conn* me-deq-ch them-enq-ch)
                  (<- conn-establisher
                      (spawn ^message-io me-deq-ch them-enq-ch)
                      remote-node)))
               #:promise? #t)))]))
    pre-setup-beh)
  (spawn ^netlayer))
