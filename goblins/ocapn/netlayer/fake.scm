(define-module (goblins ocapn netlayer fake)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module ((goblins core) #:renamer (lambda (x) (if (eq? x '$) '$C x)))
  #:use-module (goblins vat)
  #:use-module (goblins inbox)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins ocapn structs-urls)
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

(define (make-message-reader incoming-ch)
  (lambda (unmarshallers)
    (define msg (get-message incoming-ch))
    (syrup-decode msg #:unmarshallers unmarshallers)))

(define (make-message-writer outgoing-ch)
  (lambda (msg marshallers)
    (put-message outgoing-ch
                 (syrup-encode msg #:marshallers marshallers))))

(define (^fake-netlayer _bcom our-name network new-conn-ch)
  (define our-location (make-ocapn-machine 'fake our-name #f))
  (define (start-listening conn-establisher)
    (syscaller-free-fiber
     (lambda ()
       ;; TODO: Insert shutdown code nere
       (while #t
         (match-let ((('*incoming-new-conn* them-enq-ch me-deq-ch)
                      (get-message new-conn-ch)))
           (<-np-extern conn-establisher
                        (make-message-reader me-deq-ch)
                        (make-message-writer them-enq-ch)
                        #t))))))

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
    (same-machine-location? our-location loc)]
       [(connect-to remote-machine)
    (match remote-machine
      (($ <ocapn-machine> 'fake name #f)
       (on (<- network 'connect-to name)
           (match-lambda
                 (('*outgoing-new-conn* me-deq-ch them-enq-ch)
          (<- conn-establisher
              (make-message-reader me-deq-ch)
              (make-message-writer them-enq-ch)
              #f)))
           #:promise? #t)))]))
    pre-setup-beh)
  (spawn ^netlayer))
