(use-modules (goblins)
             (goblins ocapn netlayer base-port)
             (goblins ocapn netlayer utils)
             (goblins ocapn captp)
             (goblins ocapn structs-urls)
             (goblins utils random-name)
             (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 curried-definitions))

(define* (^testuds-netlayer bcom netlayers-dir
                            #:key (machine-id (random-name 32)))
  (define our-location
    (make-ocapn-machine 'testuds machine-id '()))
  (define (sock-filename-for-id id)
    (string-append netlayers-dir file-name-separator-string id
                   ".sock"))
  (define our-sock-filename
    (sock-filename-for-id machine-id))
  (define our-server-sock
    (make-server-unix-domain-socket (sock-filename-for-id machine-id)))
  (define (incoming-accept)
    (match (accept our-server-sock SOCK_NONBLOCK)
      ((client . addr)
       (setvbuf client 'block 1024)
       ;; (As said in the Fibers manual:)
       ;; Disable Nagle's algorithm.  We buffer ourselves.
       ;; (setsockopt client IPPROTO_TCP TCP_NODELAY 1)
       client)))
  (define (outgoing-connect-location location)
    (unless (eq? (ocapn-machine-transport location) 'testuds)
      (error "Wrong netlayer! Expected testuds" location))
    (make-client-unix-domain-socket (sock-filename-for-id
                                     (ocapn-machine-address location))))
  (^base-port-netlayer bcom our-location
                       incoming-accept outgoing-connect-location))

(define netlayers-dir "/tmp/netlayers")
(unless (file-exists? netlayers-dir)
  (mkdir netlayers-dir))

;;; Evaluate the above, then copy-paste the below to the REPL
(begin
  (define a-vat (spawn-vat))
  (define-vat-run a-run a-vat)
  (define b-vat (spawn-vat))
  (define-vat-run b-run b-vat)

  (define ((^greeter _bcom my-name) your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name))

  (define alice
    (a-run (spawn ^greeter "Alice")))

  (define a-nl (a-run (spawn ^testuds-netlayer netlayers-dir)))
  (define a-mycapn (a-run (spawn-mycapn a-nl)))
  (define b-nl (b-run (spawn ^testuds-netlayer netlayers-dir)))
  (define b-mycapn (b-run (spawn-mycapn b-nl)))
  (define a-loc (a-run ($ a-nl 'our-location)))
  (define b-loc (b-run ($ b-nl 'our-location)))
  (a-run (on ($ a-mycapn 'connect-to-machine b-loc)
             (lambda (loc)
               (pk 'horray-connected-to loc)))))

;; And if you're bold enough to try sturdyrefs, try this too
(begin
  (define alice-sref (a-run ($ a-mycapn 'register alice 'testuds)))
  (b-run (on (<- ($ b-mycapn 'enliven alice-sref) "Bob")
             (lambda (greets)
               (pk 'heard-back greets)))))
