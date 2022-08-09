;;; Copyright 2021-2022 Christine Lemmer-Webber
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

(define-module (goblins ocapn netlayer onion)
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
  #:use-module (goblins ocapn structs-urls)
  #:use-module (goblins ocapn netlayer utils)
  #:use-module (goblins ocapn netlayer onion-socks)
  #:use-module (goblins contrib syrup)
  #:export (new-onion-netlayer
            restore-onion-netlayer))

(define _void *unspecified*)

(define (line-delimited-ports->channels ip op)
  (define-values (in-enq-ch in-deq-ch in-stop?)
    (spawn-delivery-agent))
  (define-values (out-enq-ch out-deq-ch out-stop?)
    (spawn-delivery-agent))

  (syscaller-free-fiber
   (lambda ()
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
            (put-message in-enq-ch incoming-str)
            (lp '()))]  ; safe to recur, handle-event is called in tail position
         ;; keep on bufferin'
         [char (lp (cons char buf))]))))

  (syscaller-free-fiber
   (lambda ()
     (let lp ()
       (match (get-message out-deq-ch)
         ;; we're done
         ['close
          (close-input-port ip)
          (close-output-port op)]
         [(? bytevector? msg)
          (display msg op)
          (display "\r\n" op)
          (flush-output-port op)
          (lp)]))))

  (values in-deq-ch out-enq-ch))

(define (tor-control-connect-unix path)
  (define sock
    (socket PF_UNIX SOCK_STREAM 0))
  (connect sock AF_UNIX path)
  (line-delimited-ports->channels sock sock))

(define (build-path . args)
  (string-join args file-name-separator-string))

(define default-goblins-tor-dir
  (build-path (getenv "HOME") ".cache" "goblins" "tor"))

(define default-tor-socks-path
  (build-path default-goblins-tor-dir "tor-socks-sock"))

(define default-tor-control-path
  (build-path default-goblins-tor-dir "tor-control-sock"))

(define default-tor-ocapn-socks-dir
  (build-path default-goblins-tor-dir "ocapn-sockets"))

(define (expect-250-ok tor-in-ch)
  (match (get-message tor-in-ch)
    ["250 OK" 'ok]
    [something-else
     (error 'wrong-response "Failed to authenticate, got: ~a" something-else)]))

(define (setup-ocapn-io tor-control-path tor-ocapn-socks-dir)
  ;; Set up the temporary directory and paths we'll be using for this
  ;; captp process
  (unless (file-exists? tor-ocapn-socks-dir)
    ;; TODO: Make this recursive?
    (mkdir tor-ocapn-socks-dir))
  (define ocapn-socks-dir
    (mkdtemp (format #f "~a/XXXXXX" tor-ocapn-socks-dir)))
  (define ocapn-socks-path
    (string-append ocapn-socks-dir file-name-separator-string "ocapn.sock"))

  (define-values (tor-in-ch tor-out-ch)
    (tor-control-connect-unix tor-control-path))
  
  (put-message tor-out-ch "AUTHENTICATE")
  (expect-250-ok tor-in-ch)

  (define ocapn-sock-listener (unix-socket-listen ocapn-sock-path))
  (values ocapn-sock-dir ocapn-sock-path ocapn-sock-listener
          tor-in-ch tor-out-ch))

(define (new-tor-connection tor-control-path tor-ocapn-socks-dir)
  (define-values (ocapn-sock-dir
                  ocapn-sock-path ocapn-sock-listener
                  tor-in-ch tor-out-ch)
    (setup-ocapn-io tor-control-path tor-ocapn-socks-dir))

  (put-message tor-out-ch
               (format #f "ADD_ONION NEW:ED25519-V3 PORT=9045,unix:~a"
                       ocapn-sock-path))

  (define service-id
    (let ([response (get-message tor-in-ch)])
      (match (string-match "^250-ServiceID=(.+)$" response)
        [(? regexp-match? rxm)
         (regexp-substitute #f rxm 1)]
        [#f (error 'wrong-response "Expected ServiceId response, got:"
                   response)])))
  (define private-key
    (let ([response (get-message tor-in-ch)])
      (match (string-match "^250-PrivateKey=(.+)$" response)
        [(? regexp-match? rxm)
         (regexp-substitute #f rxm 1)]
        [#f (error 'wrong-response "Expected PrivateKey response, got:"
                   response)])))

  (expect-250-ok tor-in-ch)
  
  (values ocapn-sock-dir ocapn-sock-path ocapn-sock-listener service-id private-key))

(define (restore-tor-connection tor-control-path tor-ocapn-socks-dir
                                private-key service-id)
  (define-values (ocapn-sock-dir
                  ocapn-sock-path ocapn-sock-listener
                  tor-in-ch tor-out-ch)
    (setup-ocapn-io tor-control-path tor-ocapn-socks-dir))
  (put-message tor-out-ch
               (format #f "ADD_ONION ~a PORT=9045,unix:~a"
                       private-key
                       ocapn-sock-path))
  (define returned-service-id
    (let ([response (get-message tor-in-ch)])
      (match (string-match "^250-ServiceID=(.+)$" response)
        [(? regexp-match? rxm)
         (regexp-substitute #f rxm 1)]
        [#f (error 'wrong-response "Expected ServiceId response, got:"
                   response)])))
  (unless (equal? service-id returned-service-id)
    (error "Got the wrong service-id; expected ~a but got ~a"
           service-id returned-service-id))
  (expect-250-ok tor-in-ch)

  (values ocapn-sock-dir ocapn-sock-path ocapn-sock-listener))

(define (^onion-netlayer bcom our-location ocapn-sock-listener
                         tor-socks-path do-cleanup)
  (define shutdown-time (make-condition))
  (define (start-listen-thread conn-establisher)
    (define (handle-ocapn-sock-listen)
      (define-values (ip op)
        (unix-socket-accept ocapn-sock-listener))
      (define-values (read-message write-message)
        (read-write-procs ip op))
      (<-np-extern conn-establisher read-message write-message #t))
    (syscaller-free-fiber
     (lambda ()
       (dynamic-wind
         _void
         (lambda ()
           (let lp ()
             (choice-operation
              ;; If we shutdown, we won't loop
              shutdown-time
              ;; Otherwise, if a new connection is ready, let's go
              (wrap-operation ocapn-sock-listener
                              (lambda _
                                (handle-ocapn-sock-listen)
                                (lp))))))
         do-cleanup))))

  (define base-beh
    (methods
     [(netlayer-name) 'onion]
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
      (unless (eq? (ocapn-machine-transport remote-machine) 'onion)
        (error "Not an onion ocapn machine:" remote-machine))
      (let* ((address (ocapn-machine-address remote-machine))
             ;; hacky way to start the connection in another thread but with
             ;; working promise machinery
             (connect-vat (spawn-vat))
             (^start-conn
              (lambda (_bcom)
                (lambda ()
                  (define-values (ip op)
                    (unix-socket-connect tor-socks-path))
                  (onion-socks5-setup! ip op (string-append address ".onion"))
                  (define-values (read-message write-message)
                    (read-write-procs ip op))
                  (<- conn-establisher read-message write-message #f))))
             (start-conn (connect-vat 'spawn ^start-conn)))
        (<- start-conn))]))
  pre-setup-beh)

(define (_finish-setup-onion private-key service-id tor-socks-path
                             ocapn-tmp-dir ocapn-sock-path ocapn-sock-listener)
  ;; TODO: Cleanup tor subprocess also.
  (define (do-cleanup)
    (unix-socket-close-listener ocapn-sock-listener)
    (delete-file ocapn-sock-path)
    (rmdir ocapn-tmp-dir))

  (define our-location
    (make-ocapn-machine 'onion service-id #f))

  (values (spawn ^onion-netlayer our-location ocapn-sock-listener
                 tor-socks-path do-cleanup)
          private-key service-id))

;; TODO: I guess this really *should* return one value to its
;; continuation, and the pre-setup-beh above should also support
;; our-location
(define* (new-onion-netlayer
          #:key
          [tor-control-path default-tor-control-path]
          [tor-socks-path default-tor-socks-path]
          [tor-ocapn-socks-dir default-tor-ocapn-socks-dir])
  (define-values (ocapn-tmp-dir ocapn-sock-path ocapn-sock-listener service-id private-key)
    (new-tor-connection tor-control-path tor-ocapn-socks-dir))

  (_finish-setup-onion private-key service-id tor-socks-path
                       ocapn-tmp-dir ocapn-sock-path ocapn-sock-listener))

(define* (restore-onion-netlayer
          service-id private-key
          #:key [tor-control-path default-tor-control-path]
          [tor-socks-path default-tor-socks-path]
          [tor-ocapn-socks-dir default-tor-ocapn-socks-dir])
  (define-values (ocapn-tmp-dir ocapn-sock-path ocapn-sock-listener)
    (restore-tor-connection tor-control-path tor-ocapn-socks-dir
                            private-key service-id))
  (_finish-setup-onion private-key service-id tor-socks-path
                       ocapn-tmp-dir ocapn-sock-path ocapn-sock-listener))


