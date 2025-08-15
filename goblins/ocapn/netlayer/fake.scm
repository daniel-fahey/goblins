;;; Copyright 2021-2022 Christine Lemmer-Webber
;;; Copyright 2022-2025 Jessica Tallon
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
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module ((goblins core) #:hide ($))
  #:use-module ((goblins core) #:select ($) #:prefix $)
  #:use-module (goblins vat)
  #:use-module (goblins inbox)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib io)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins contrib syrup)
  #:use-module (ice-9 match)
  #:export (^fake-network ^fake-netlayer))

(define (^fake-network _bcom)
  (define routes (spawn ^ghash))
  (methods
   [(register name new-conn-ch)
    ($$ routes 'set name new-conn-ch)]
   [(connect-to name)
    (define connection-ch
      ($$ routes 'ref name))
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

(define (^message-io bcom incoming-ch outgoing-ch)
  (define incoming-io (spawn ^io incoming-ch))
  (define outgoing-io (spawn ^io outgoing-ch))
  (define halt? (make-condition))

  (define (read-message ch)
    (perform-operation
     (choice-operation
      (get-operation ch)
      (wrap-operation (wait-operation halt?)
                      (lambda () the-eof-object)))))

  (define halted-beh
    (methods
     [(read-message unmarshallers) the-eof-object]
     [(write-message msg marshallers) *unspecified*]
     [(halt) *unspecified*]))
  (define main-beh
    (methods
     [(halt)
      ;; Propagate the halt to the otherside...
      (<-np outgoing-io
            (lambda (ch)
              (put-message ch the-eof-object)
              *unspecified*))
      ;; Signal the condition to halt and tell the io actors to stop.
      (signal-condition! halt?)
      (<-np incoming-io 'halt)
      (<-np outgoing-io 'halt)
      ;; Become some defunct behavior.
      (bcom halted-beh)]
     [(read-message unmarshallers)
      (<- incoming-io
          (lambda (ch)
            (match (read-message ch)
              ((? eof-object? eof) eof)
              (msg (syrup-decode msg #:unmarshallers unmarshallers)))))]
     [(write-message msg marshallers)
      (<-np outgoing-io
            (lambda (ch)
              (put-message ch (syrup-encode msg #:marshallers marshallers))
              *unspecified*))]))
  main-beh)


(define (^fake-netlayer _bcom our-name network new-conn-ch)
  (define our-location (make-ocapn-peer 'fake our-name #f))
  (define-values (halted-vow halted-resolver)
    (spawn-promise-and-resolver))
  (define new-connection-io (spawn ^io new-conn-ch))
  (define (start-listening conn-establisher)
    (on (<- new-connection-io get-message)
        (match-lambda
          (('*incoming-new-conn* them-enq-ch me-deq-ch)
           (define message-io
             (spawn ^message-io me-deq-ch them-enq-ch))
           (on halted-vow
               (lambda _
                 (<-np message-io 'halt)))
           (<-np-extern conn-establisher message-io #f)))
        #:finally
        (lambda ()
          (start-listening conn-establisher))))

  (define* (^netlayer bcom #:optional conn-establisher)
    (methods
     [(netlayer-name) 'fake]
     [(self-location? loc)
      (same-peer-location? our-location loc)]
     [(our-location) our-location]
     [(setup conn-establisher)
      (start-listening conn-establisher)
      (bcom (^netlayer bcom conn-establisher))]
     [(halt) ($$ halted-resolver 'fulfill #t)]
     [(connect-to remote-peer)
      (match remote-peer
        (($ <ocapn-peer> 'fake name _)
         (on (<- network 'connect-to name)
             (match-lambda
               (('*outgoing-new-conn* me-deq-ch them-enq-ch)
                (define message-io
                  (spawn ^message-io me-deq-ch them-enq-ch))
                (on halted-vow
                    (lambda _
                      (<-np message-io 'halt)))
                (<- conn-establisher message-io remote-peer)))
             #:promise? #t)))]))
  (spawn ^netlayer))
