(use-modules (goblins)
             (goblins ocapn captp)
             (goblins ocapn ids)
             (goblins ocapn netlayer testuds)
             (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 curried-definitions))

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
