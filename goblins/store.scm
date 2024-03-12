(define-module (goblins store)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 atomic)
  #:export (<persistence-store>
            make-persistence-store
            persistence-store-read-proc
            persistence-store-save-proc

            make-memory-store))

(define-record-type <persistence-store>
  (make-persistence-store read-proc save-proc)
  persistence-store?
  (read-proc persistence-store-read-proc)
  (save-proc persistence-store-save-proc))

;; Basic in memory persistence store
(define (make-memory-store)
  (define stored-portraits (make-atomic-box #f))
  (define stored-roots (make-atomic-box #f))
  (define (memory-read-proc)
    (values (atomic-box-ref stored-portraits)
            (atomic-box-ref stored-roots)))
  (define* (memory-save-proc delta-portraits #:optional roots)
    (let ((saved-portraits (atomic-box-ref stored-portraits)))
      (if saved-portraits
          (begin
            (hash-for-each
             (lambda (key value)
               (hashq-set! saved-portraits key value))
             delta-portraits)
            (atomic-box-set! stored-portraits saved-portraits))
          (atomic-box-set! stored-portraits delta-portraits)))
    (when roots
      (atomic-box-set! stored-roots roots)))
  (make-persistence-store memory-read-proc memory-save-proc))
