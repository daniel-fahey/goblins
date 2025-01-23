(define-module (tests ocapn netlayer test-websocket)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib nonce-registry)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn netlayer websocket)
  #:use-module (goblins persistence-store memory)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils))

(test-begin "test-websocket-netlayer")

(define-actor (^greeter _bcom my-name)
  (lambda (your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name)))

(define server-vat (spawn-vat #:name "server-vat"))
(define client-vat (spawn-vat #:name "client-vat"))

(test-equal "Enliven far sturdyref and use it from client->server with TLS encryption"
  #(ok "Hello Alice, my name is Bob!")
  (let* ((server-netlayer (with-vat server-vat (spawn ^websocket-netlayer)))
         (server-mycapn (with-vat server-vat (spawn-mycapn server-netlayer)))
         (bob (with-vat server-vat (spawn ^greeter "Bob")))
         (bob-sref (with-vat server-vat
                     ($ server-mycapn 'register bob 'websocket)))
         (client-netlayer (with-vat client-vat
                            (spawn ^websocket-netlayer
                                   #:verify-certificates? #f)))
         (client-mycapn (with-vat client-vat (spawn-mycapn client-netlayer))))
    (resolve-vow-and-return-result
     client-vat
     (lambda ()
       (define bob-vow (<- client-mycapn 'enliven bob-sref))
       (<- bob-vow "Alice")))))

(test-equal "Enliven far sturdyref and use it from client->server without encryption"
  #(ok "Hello Alice, my name is Bob!")
  (let* ((server-netlayer (with-vat server-vat
                            (spawn ^websocket-netlayer #:encrypted? #f)))
         (server-mycapn (with-vat server-vat (spawn-mycapn server-netlayer)))
         (bob (with-vat server-vat (spawn ^greeter "Bob")))
         (bob-sref (with-vat server-vat
                     ($ server-mycapn 'register bob 'websocket)))
         (client-netlayer (with-vat client-vat
                            (spawn ^websocket-netlayer #:encrypted? #f)))
         (client-mycapn (with-vat client-vat (spawn-mycapn client-netlayer))))
    (resolve-vow-and-return-result
     client-vat
     (lambda ()
       (define bob-vow (<- client-mycapn 'enliven bob-sref))
       (<- bob-vow "Alice")))))

;; Test persistence
(test-equal "Enliven far sturdyref and use it from client->server with persistence"
  #(ok "Hello Alice, my name is Bob!")
  (let*-values (((server-store) (make-memory-store))
                ((env)
                 (persistence-env-compose
                  (namespace-env
                   '(tests ocapn netlayer test-websocket)
                   ^greeter)
                  nonce-registry-env
                  captp-env
                  websocket-netlayer-env))
                ((server-vat server-mycapn server-netlayer)
                 (spawn-persistent-vat
                  env
                  (lambda ()
                    (define server-netlayer (spawn ^websocket-netlayer))
                    (values (spawn-mycapn server-netlayer)
                            server-netlayer))
                  server-store)))
    ;; Stop WebSocket netlayer, then stop the vat.
    (resolve-vow-and-return-result server-vat
                                   (lambda () (<- server-netlayer 'halt)))
    (vat-halt! server-vat)
    ;; Restore the vat and make sure the netlayer still works.
    (let*-values (((server-vat server-mycapn server-netlayer)
                   (spawn-persistent-vat
                    env
                    (lambda ()
                      (error "should be restoring from memory"))
                    server-store))
                  ((bob) (with-vat server-vat (spawn ^greeter "Bob")))
                  ((bob-sref)
                   (with-vat server-vat
                     ($ server-mycapn 'register bob 'websocket)))
                  ((client-netlayer)
                   (with-vat client-vat
                     (spawn ^websocket-netlayer #:verify-certificates? #f)))
                  ((client-mycapn)
                   (with-vat client-vat
                     (spawn-mycapn client-netlayer))))
      (resolve-vow-and-return-result
       client-vat
       (lambda ()
         (define bob-vow (<- client-mycapn 'enliven bob-sref))
         (<- bob-vow "Alice"))))))

(test-end)
