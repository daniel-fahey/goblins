;; Basically import all modules that are valid things to import
;; on hoot. This will check for undefined or incompatible hoot
;; code even if it's not used.
(use-modules (goblins)
             (goblins utils hashmap)
             (goblins utils ghash)
             (goblins utils crypto)
             (goblins ocapn ids)
             (goblins ocapn captp)
             (goblins ocapn netlayer fake)
             (goblins ocapn netlayer websocket)
             (goblins persistence-store memory)
             (goblins persistence-store local-storage)
             (goblins actor-lib cell)
             (goblins actor-lib common)
             (goblins actor-lib facet)
             (goblins actor-lib inbox)
             (goblins actor-lib io)
             (goblins actor-lib joiners)
             (goblins actor-lib methods)
             (goblins actor-lib on)
             (goblins actor-lib opportunistic)
             (goblins actor-lib pubsub)
             (goblins actor-lib simple-mint)
             (goblins actor-lib swappable)
             (goblins actor-lib queue)
             (goblins actor-lib ring-buffer)
             (goblins actor-lib ticker)
             (goblins actor-lib timers)
             (goblins actor-lib ward)
             (tests utils)
             (fibers)
             (fibers channels))

;; Do a very brief test using the fake netlayer check CapTP
(define vat (spawn-vat))
(define cell-contents-over-captp-vow
  (with-vat vat
    (define network (spawn ^fake-network))

    (define a-new-conn-ch (make-channel))
    (define a-loc (string->ocapn-id "ocapn://a.fake"))
    (define b-new-conn-ch (make-channel))
    (define b-loc (string->ocapn-id "ocapn://b.fake"))

    (define a-netlayer
      (spawn ^fake-netlayer "a" network a-new-conn-ch))
    (define b-netlayer
      (spawn ^fake-netlayer "b" network b-new-conn-ch))

    (define a-mycapn
      (spawn-mycapn a-netlayer))
    (define b-mycapn
      (spawn-mycapn b-netlayer))
    ($ network 'register "a" a-new-conn-ch)
    ($ network 'register "b" b-new-conn-ch)

    (define a-cell (spawn ^cell 'i-am-on-a))
    (define a-cell-sref ($ a-mycapn 'register a-cell 'fake))
    (define a-remote-refr-vow ($ b-mycapn 'enliven a-cell-sref))
    (<- a-remote-refr-vow)))

(format #t "Captp: ~a\n"
        (resolve-vow-and-return-result
         vat
         (lambda ()
           cell-contents-over-captp-vow)))
