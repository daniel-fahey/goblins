(define-module (goblins tests test-stage1)
  #:use-module (goblins stage1)
  #:use-module (srfi srfi-64))

(test-begin "test-goblins-stage1")

(define am (make-whactormap))

(define (^greeter _bcom my-name)
  (lambda (your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name)))

(define alice
  (actormap-direct-run!
   am
   (lambda ()
     (spawn ^greeter "Alice"))))

(test-equal "Hello Bob, my name is Alice!"
  (actormap-direct-run!
   am
   (lambda ()
     ($ alice "Bob"))))

(test-end "test-goblins-stage1")
