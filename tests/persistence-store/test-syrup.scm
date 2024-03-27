(define-module (tests persistence-store test-syrup)
  #:use-module (goblins)
  #:use-module (goblins core-types)
  #:use-module (goblins persistence-store syrup)
  #:use-module (srfi srfi-64))

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
(define-values (portraits values)
  (actormap-take-portrait am env alice bob))

(define filename (tmpnam))
(define store
  (make-syrup-store filename))

(actormap-save-to-store! am env store alice bob)

;; Check we can get a single value from the store
(define read-from-store
  (persistence-store-read-proc store))

(test-equal "Single object lookup works as expected"
  (read-from-store 'object-portrait 0)
  (make-portrait-record
   'object
   ;; Object name
   '(((tests persistence-store test-syrup) ^greeter)
     ;; Debug name
     ^greeter
     ;; Version
     0
     ;; Data
     ("Alice" 0))))

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

(test-end "test-syrup-store")
