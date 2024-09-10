(define-module (tests persistence-store test-syrup)
  #:use-module (goblins)
  #:use-module (goblins core-types)
  #:use-module (goblins persistence-store syrup)
  #:use-module (goblins ocapn marshalling)
  #:use-module (goblins contrib syrup)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(test-begin "test-syrup-store")

(define-actor (^greeter bcom our-name #:optional [called 0])
  (lambda (their-name)
    (bcom (^greeter bcom our-name (+ 1 called))
          (format #f "Hello ~a, my name is ~a (called: ~a)"
                  their-name our-name called))))

(define env
  (make-persistence-env
   `((((tests persistence-store test-syrup) ^greeter) ,^greeter))))

(define am (make-actormap))
(define alice (actormap-spawn! am ^greeter "Alice"))
(define bob (actormap-spawn! am ^greeter "Bob"))
(define-values (portraits slots)
  (actormap-take-portrait am env alice bob))

(define filename (tmpnam))
(define store
  (make-syrup-store filename))

(actormap-save-to-store! am env store alice bob)

;; Check we can get a single value from the store
(define read-from-store
  (persistence-store-read-proc store))

(test-equal "Single object lookup works as expected"
  (read-from-store 'object-portrait (car slots))
  '(((tests persistence-store test-syrup) ^greeter)
    ;; Debug name
    ^greeter
    ;; Version
    0
    ;; Data
    ("Alice" 0)))

;; Increment alice's counter a few times
(actormap-poke! am alice "Carol")
(actormap-poke! am alice "Carol")
(actormap-poke! am alice "Carol")
(actormap-save-to-store! am env store alice bob)

;; To ensure they're fully read in, lets make a new store to the
;; same file. Obviously this is not want you ever want to do!
(define am* (make-actormap))
(define store*
  (make-syrup-store filename))
(define-values (alice* bob*)
  (actormap-restore-from-store! am* env store*))

(test-equal "Check restored alice is the same as saved alice"
  "Hello Carol, my name is Alice (called: 3)"
  (actormap-peek am* alice* "Carol"))
(test-equal "Check restored bob is the same as saved bob"
  "Hello Carol, my name is Bob (called: 0)"
  (actormap-peek am* bob* "Carol"))

;; Check upgrading from a version without aurie-vat-id and root version
(define-record-type <v0-portrait-graph>
  (make-v0-portrait-graph version portraits slots)
  portrait-graph?
  (version portrait-graph-version)
  (portraits portrait-graph-portraits)
  (slots portrait-graph-slots))

(define-values (marshaller::v0-portrait-graph unmarshaller::v0-portrait-graph)
  (make-marshallers <v0-portrait-graph> #:name '<portrait-graph>))

(define v0-portrait-graph (make-v0-portrait-graph 0 portraits slots))
(define filename (tmpnam))
(call-with-output-file filename
  (lambda (port)
    (syrup-write v0-portrait-graph port
                 #:marshallers (list marshaller::v0-portrait-graph))))

(define store (make-syrup-store filename))

(define read-from-store
  (persistence-store-read-proc store))

(test-assert "Can read version 0 portrait graph data"
  (match (call-with-values (lambda () (read-from-store 'graph-and-slots)) list)
    [(aurie-vat-id roots-version portrait slots) #t]))

(test-end "test-syrup-store")
