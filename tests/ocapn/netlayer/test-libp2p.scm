(define-module (tests ocapn netlayer test-libp2p)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer libp2p)
  #:use-module (goblins ocapn netlayer utils)
  #:use-module (goblins persistence-store memory)
  #:use-module (tests utils)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match))

(test-begin "test-libp2p-netlayer")

;; The libp2p netlayer communicates over unix sockets to the daemon. That makes
;; it somewhat difficult to test, in order to do that we need to make a unix
;; socket it can connect to and pretend to be the daemon and check it's sending
;; what it's supposed to...
(define test-vat (spawn-vat #:name "test"))
(define control-socket-path (random-tmp-filename "/tmp"))
(define control-socket-sock
  (make-server-unix-domain-socket control-socket-path))

(define libp2p-path
  (mkdtemp "/tmp/guile-goblins-XXXXXX"))
(define libp2p-store (make-memory-store))
(define-values (libp2p-vat libp2p-netlayer)
  (spawn-persistent-vat
   libp2p-netlayer-env
   (lambda ()
     (spawn ^libp2p-netlayer
            #:control-path control-socket-path
            #:path libp2p-path))
   libp2p-store
   #:name "libp2p-netlayer"))

;; The libp2p should try to connect to the control socket
;; lets accept that and spawn a line delimited port
;; TODO: could block, make it timeout...
(define control-socket-io
  (with-vat test-vat
    (spawn ^line-delimited-port
           (match (accept control-socket-sock O_NONBLOCK)
             [(client . addr) client]))))

(define (read-line! io)
  (match (resolve-vow-and-return-result
          test-vat
          (lambda ()
            (<- io 'read-line)))
    [#(ok line) line]))

(define (parse-line str)
  (define parts
    (string-split str #\space))
  (define parsed
    (map
     (lambda (part)
       (if (string-contains part ":")
           (string-split part #\:)
           part))
     parts))
  ;; Remove empty strings...
  (filter (lambda (part)
            (if (string? part)
                (not (string-null? part))
                #t))
          parsed))

(test-assert "Check libp2p sends correct NEW message"
  (match (parse-line (read-line! control-socket-io))
    [("NEW"
      ("incoming" incoming-path) ("outgoing" outgoing-path)
      ("protocol" "ocapn") ("version" "1.0.0"))
     (and (string-prefix? libp2p-path incoming-path)
          (string-prefix? libp2p-path outgoing-path))]
    [something-else #f]))

(define provided-private-key "this-is-our-super-secret-pk")
(define provided-location "/ip4/1.2.3.4/udp/6000/p2p/node-id-is-here")
(with-vat test-vat
  (<-np control-socket-io 'write-line
        (format #f "address:~a private-key:~a\n"
                provided-location provided-private-key)))

(test-equal "Check libp2p location is correct"
  #(ok "ocapn://node-id-is-here.libp2p?multiaddr=%2Fip4%2F1.2.3.4%2Fudp%2F6000%2Fp2p%2Fnode-id-is-here")
  (resolve-vow-and-return-result
   libp2p-vat
   (lambda ()
     (on (<- libp2p-netlayer 'our-location) ocapn-id->string #:promise? #t))))

;; Check that we restore correctly and provide the key and location
(define-values (libp2p-vat* libp2p-netlayer*)
  (spawn-persistent-vat
   libp2p-netlayer-env
   (lambda ()
     (error "should be read from store, not here"))
   libp2p-store
   #:name "Restore libp2p vat"))


;; TODO: could block, make it timeout...
;; IMPORTANT: TODO: There's currently a "bug" in aurie where old objects
;; are not cleaned up properly... this results in both the ^libp2p-FRESH
;; and ^libp2p-SETUP objects remaining around when we really only want
;; to keep ^libp2p-SETUP.
;;
;; Unfortunately due to this bug both objects are restored. It just so
;; happens it always spawns the fresh one first, then setup. To deal
;; with that we throw away the fresh one. This is kinda brittle and
;; awful but it's take a lot to work around what isn't a libp2p netlayer
;; but, an aurie deficiency...
(accept control-socket-sock O_NONBLOCK)

(define control-socket-io*
  (with-vat test-vat
    (spawn ^line-delimited-port
           (match (accept control-socket-sock O_NONBLOCK)
             [(client . addr) client]))))

;; FIXME: This test is hanging on some machines.
(test-skip "Check libp2p sends correct NEW message with restored private key")
(test-assert "Check libp2p sends correct NEW message with restored private key"
  (match (parse-line (read-line! control-socket-io*))
    [("NEW"
      ("incoming" incoming-path) ("outgoing" outgoing-path)
      ("protocol" "ocapn") ("version" "1.0.0")
      ("private-key" key) ("address" addr))
     (and (string-prefix? libp2p-path incoming-path)
          (string-prefix? libp2p-path outgoing-path)
          (string=? key provided-private-key)
          (string-prefix? addr provided-location))]
    [something-else #f]))

(test-end "test-libp2p-netlayer")
