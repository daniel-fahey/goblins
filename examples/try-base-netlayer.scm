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
  (define b-vat (spawn-vat))

  (define ((^greeter _bcom my-name) your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name))

  (define alice
    (with-vat a-vat (spawn ^greeter "Alice")))

  (define a-nl (with-vat a-vat (spawn ^testuds-netlayer netlayers-dir)))
  (define a-mycapn (with-vat a-vat (spawn-mycapn a-nl)))
  (define b-nl (with-vat b-vat (spawn ^testuds-netlayer netlayers-dir)))
  (define b-mycapn (with-vat b-vat (spawn-mycapn b-nl)))
  (define a-loc (with-vat a-vat ($ a-nl 'our-location)))
  (define b-loc (with-vat b-vat ($ b-nl 'our-location)))
  (with-vat a-vat
    (on ($ a-mycapn 'connect-to-peer b-loc)
        (lambda (loc)
          (pk 'horray-connected-to loc)))))

;; And if you're bold enough to try sturdyrefs, try this too
(begin
  (define alice-sref (with-vat a-vat ($ a-mycapn 'register alice 'testuds)))
  (with-vat b-vat
    (on (<- ($ b-mycapn 'enliven alice-sref) "Bob")
        (lambda (greets)
          (pk 'heard-back greets)))))
