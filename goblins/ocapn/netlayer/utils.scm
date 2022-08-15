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

(define-module (goblins ocapn netlayer utils)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins contrib syrup)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib ward)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins utils random-name)
  #:export (read-write-procs
            random-tmp-filename
            make-server-unix-domain-socket
            make-client-unix-domain-socket
            ^unix-socket))

(define (read-write-procs ip op)
  (define (read-message unmarshallers)
    (syrup-read ip #:unmarshallers unmarshallers))
  (define (write-message msg marshallers)
    (syrup-write msg op #:marshallers marshallers)
    (flush-output-port op))
  (values read-message write-message))

(define* (random-tmp-filename base-directory
                              #:key
                              (random-len 16)
                              (format-name
                               (lambda (name)
                                 (string-append "tmp-" name))))
  "Generate a new filename in base-directory, returns the filename
including the BASE-DIRECTORY

Will detect if a collision exists, but doesn't take action to
create or claim the file.  Extremely unlikely race conditions could
exist between this time, but they are really extremely unlikely."
  (let lp ()
    (define new-filename
      (string-append base-directory file-name-separator-string
                     (format-name (random-name random-len))))
    (if (file-exists? new-filename)
        (lp)             ; try again
        new-filename)))

(define* (make-server-unix-domain-socket path #:optional (listen-backlog 1024))
  (let ((sock (socket PF_UNIX SOCK_STREAM 0)))   ; open unix domain socket
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)  ; allow socket reuse
    (fcntl sock F_SETFD FD_CLOEXEC)
    (bind sock AF_UNIX path)
    (fcntl sock F_SETFL
           (logior O_NONBLOCK
                   (fcntl sock F_GETFL)))
    (sigaction SIGPIPE SIG_IGN)
    (listen sock listen-backlog)
    sock))

(define (make-client-unix-domain-socket path)
  (let ((sock (socket PF_UNIX SOCK_STREAM 0)))   ; open unix domain socket
    (connect sock AF_UNIX path)
    (fcntl sock F_SETFL
           (logior O_NONBLOCK
                   (fcntl sock F_GETFL)))
    sock))

;;;; Jessica's new line-delimited ports actors design
;;;; ================================================

;; This makes sequential operations easy in the asynchronous promise based
;; system, that is Goblins.
;;
;; The ^port object exposes several methods (queue-send, queue-recieve-byte,
;; etc.) which will queue up an operation and return a promise, the promise is
;; fulfilled when the operation has occured.
;;
;; == Operations ==
;; Each operation is a vow when run from spawn-fibrous-vow. The
;; `process-operation` function will dequeue the first operation in the queue
;; and then run that by calling it (getting the promise) and then using `on` to
;; wait until the promise has resolved.
;;
;; Once the promise has resolved, it gets the result of the operation (e.g.
;; bytes read from the port) and resolves the promise which was created for that
;; operation with the result. It will break the promise if there's an error.
;; Once that operation has completed, it will check to see if there are other
;; operations in the queue, if there are it'll emit a message which calls
;; `process-operation` again.
(define (make-port-object p)
  (define-values (warden incanter)
    (spawn-warding-pair))

  (define (make-op op)
    (define-values (promise resolver)
      (spawn-promise-values))

    (values promise
            (cons resolver op)))

  (define (^port _bcom)
    (define-cell operations
      '())

    (define (process-operation)
      (define current-operations
        ($ operations))
      (if (null? current-operations)
          (error "No operations to be processed.")
          (let* ((todo (car current-operations))
                 (resolver (car todo))
                 (operation (cdr todo)))
            (on (apply $ incanter self operation)
                (lambda (result)
                  (<-np resolver 'fulfill result))
                #:catch
                (lambda (err)
                  (<-np resolver 'break err))
                #:finally
                (lambda ()
                  (unless (null? (cdr current-operations))
                    (<-np incanter self 'process-operation))))
            ($ operations (cdr current-operations)))))

    (define (commit-operation op)
      (define current-operations
        ($ operations))
      (when (null? current-operations)
        (<-np incanter self 'process-operation))
      ($ operations (append current-operations (list op))))

    ;; These queue up an operation and give you back a promise which will be
    ;; resolved when the operation has been performed.
    (define public-beh
      (methods
       ((queue-send msg)
        (define-values (promise op)
          (make-op `(send ,msg)))
        (commit-operation op)
        promise)

       ((queue-recieve-byte)
        (define-values (promise op)
          (make-op '(read-byte)))
        (commit-operation op)
        promise)

       ((queue-recieve-message)
        (define-values (promise op)
          (make-op '(read-message)))
        (commit-operation op)
        promise)))

    (define self-beh
      (methods
       (process-operation process-operation)
       ((read-message)
        ;; What ensures that there's only one of these at any time?
        (spawn-fibrous-vow
         (lambda ()
           (let read-message ((buffer '()))
             (match (integer->char (get-u8 p))
               (#\newline (list->string (reverse buffer))) ;; stop & return.
               (#\return (read-message buffer)) ;; skip.
               (other-char (read-message (cons other-char buffer))))))))
       ((read-byte)
        (spawn-fibrous-vow
         (lambda ()
           (get-u8 p))))
       ((send msg)
        (spawn-fibrous-vow
         (lambda ()
           (cond ((integer? msg) (put-u8 p msg))
                 ((bytevector? msg) (put-bytevector p msg))))))))

    (ward warden self-beh #:extends public-beh))

  (define self (spawn ^port))
  self)

(define (^unix-socket bcom path)
  (define sock
    (socket PF_UNIX SOCK_STREAM 0))
  (connect sock AF_UNIX path)

  (define sock-port
    (make-port-object sock))

  (define defunct
    (lambda _
      (error "This connection is no longer active.")))

  (define beh
    (methods
     ((shutdown)
      (shutdown sock 0)
      (bcom defunct))
     ((ask msg #:key (replies 1))
      (beh 'send-message msg)
      (cond ((<= replies 0) #f)
            ((= replies 1) (beh 'read-message))
            (else
             (all-of* (map (lambda _ (beh 'read-message)) (iota replies))))))
     ((send-message msg)
      (cond ((integer? msg) ($ sock-port 'queue-send msg))
            ((bytevector? msg) ($ sock-port 'queue-send msg))
            ((string? msg) ($ sock-port 'queue-send (string->utf8 msg)))))

     ((read-message) ($ sock-port 'queue-recieve-message))
     ((read-byte) ($ sock-port 'queue-recieve-byte))))
  beh)
  
;; (define* (line-delimited-port->channel-pair sock)
;;   (define keep-going? #t)
;;   (define stop (make-condition))

;;   (define-values (send-enq-ch send-deq-ch send-stop)
;;     (spawn-delivery-agent))
;;   (define-values (recieve-enq-ch recieve-deq-ch recieve-stop)
;;     (spawn-delivery-agent))

;;   (define (stop-me)
;;     (signal-condition! stop))

;;   (define (close-socket-op)
;;     (wrap-operation
;;      (wait-operation stop)
;;      (lambda _
;;        (set! keep-going? #f)
;;        (shutdown sock 0)
;;        (send-stop)
;;        (recieve-stop))))

;;   (define (write-to-socket-op)
;;     (define (write-msg msg)
;;       (cond ((null? msg) #f)
;;             ((integer? msg) (put-u8 sock (pk 'out msg)))
;;             ((bytevector? msg) (put-bytevector sock (pk 'out msg)))
;;             ((string? msg) (write-msg (string->utf8 (string-append msg "\r\n"))))
;;             ((list? msg)  (write-msg (car msg)) (write-msg (cdr msg)))
;;             ((char? msg) (write-msg (char->integer msg)))))

;;     (wrap-operation
;;      (get-operation send-deq-ch)
;;      write-msg))

;;   (define (read-from-socket-op)
;;     (define* (read-message #:optional (buffer '()))
;;       (match (integer->char (get-u8 sock))
;;         (#\return (list->string (reverse buffer)))
;;         (#\newline #f)
;;         (other-char (read-message (cons other-char buffer)))))

;;     (put-operation recieve-enq-ch (pk 'in  (read-message))))

;;   (spawn-fiber
;;    (lambda ()
;;      (while keep-going?
;;        (perform-operation
;;         (choice-operation (write-to-socket-op)
;;                           (read-from-socket-op)
;;                           (close-socket-op))))))

;;   (values recieve-deq-ch send-enq-ch stop-me))
