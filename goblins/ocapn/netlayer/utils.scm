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

(define-module (goblins ocapn netlayer utils)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins inbox)
  #:use-module (goblins contrib syrup)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib ward)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins actor-lib io)
  #:use-module (goblins utils random-name)
  #:export (^captp-io
            random-tmp-filename
            make-server-unix-domain-socket
            make-client-unix-domain-socket
            ^unix-socket
            ^line-delimited-port))

(define (^captp-io _bcom port)
  (define io
    (spawn ^read-write-io
           port
           #:cleanup
           (lambda (resource)
             (close-port resource))))
  
  (methods
   [(read-message unmarshallers)
    ($ io 'read
       (lambda (ip)
         (syrup-read ip #:unmarshallers unmarshallers)))]
   [(write-message msg marshallers)
    ($ io 'write
       (lambda (op)
         (syrup-write msg op #:marshallers marshallers)
         (flush-output-port op)))]))

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


(define (^line-delimited-port _bcom port)
  (define port-io
    (spawn ^read-write-io port
           #:cleanup
           (lambda (port)
             (close-port port))))

  (methods
   [(read-line)
    ($ port-io 'read
       (lambda (ip)
         ;; Uh, I'm not sure if onion control sockets ever contain utf-8 encoded
         ;; data... I'm pretty sure no, so "forcing" a latin-1 perspective here
         (define (_read-char)
           (match (get-u8 ip)
             [(? eof-object? eof) eof]
             [char-int (integer->char char-int)]))
         (let lp ([buf '()])
           (match (_read-char)
             [(? eof-object?) 'done]
             [#\newline
              (let ((incoming-str
                     ;; Reverse and send to input channel current string
                     (string-trim-both (list->string (reverse buf)) #\return)))
                incoming-str)]
             ;; keep on bufferin'
             [char (lp (cons char buf))]))))]
   [(write-line line)
    ($ port-io 'write
       (lambda (op)
         (match line
           [(? string? msg)
            (display msg op)
            (display "\r\n" op)
            (flush-output-port op)]
           [(? bytevector? msg)
            (put-bytevector op msg)
            (display "\r\n" op)
            (flush-output-port op)])))]
   [(halt) ($ port-io 'halt)]))

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
