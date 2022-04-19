(use-modules (fibers)
             (goblins)
             (goblins actor-lib ticker)
             (ncurses curses)
             (system repl coop-server)
             (ice-9 match))

(define %repl (spawn-coop-repl-server))

(define %stdscr (initscr))

(noecho!)                ; disable echoing characters
(raw!)                   ; don't buffer input
; (keypad! %stdscr #t)     ; enable <f1>, arrow keys, etc
(start-color!)           ; turn on colors

;; Here's the design:
;;  - Wait for either:
;;    - An input to process
;;      - pass along inputs
;;    - Update time
;;      - poll coop repl
;;      - do updates
;;      - render

(define %YELLOW-N 1)
(init-pair! %YELLOW-N COLOR_YELLOW COLOR_BLACK)
(define %GREEN-N 2)
(init-pair! %GREEN-N COLOR_GREEN COLOR_BLACK)

(define (string-width-height str)
  (define lines
    (string-split str
                  (lambda (x) (eq? x #\newline))))
  (define width
    (apply max
           (map string-length lines)))
  (define height
    (length lines))
  (values width height))

(define cauldron-drawing
  "\
.--------------.
 '--   ----  -'
 /            \\
;              ;
:              :
'.            .'
  '----------'")

(define-values (cauldron-width cauldron-height)
  (string-width-height cauldron-drawing))

(define (make-string-drawing-screen str startx starty)
  (define-values (str-width str-height)
    (string-width-height str))
  (define str-len (string-length str))
  (define scr
    (newwin str-height str-width starty startx))

  (let lp ((pos 0)
           (row 0)
           (col 0))
    (if (= pos str-len)
        'done
        (match (string-ref str pos)
          ;; new row on newlines
          (#\newline
           (lp (1+ pos)
               (1+ row)
               0))
          ;; spaces are skipped
          (#\space
           (lp (1+ pos)
               row
               (1+ col)))
          (char
           (addch scr (color %YELLOW-N (bold char))
                  #:x col #:y row)
           (lp (1+ pos)
               row
               (1+ col))))))
  scr)

(use-modules (goblins actor-lib methods))

;;; cauldron and bubbles
(define bubble-max-height 10)

(define (randrange min max)
  (+ min (random (- max min))))

(define (^bubble bcom ticky)
  (define bubble-lifetime
    (randrange 2 bubble-max-height))
  (define (modify-drift drift)
    (define drift-it
      (randrange -1 2))
    (max -1 (min 1 (+ drift drift-it))))
  (define raise-delay (randrange 10 30))
  (let update ((x (randrange 2 (- cauldron-width 2)))
               (y 0)
               (time-till-raise raise-delay)
               (drift 0))
    (define time-to-live
      (- bubble-lifetime y))
    (define bubble-shape
      (cond
       ;; big bubbles
       [(>= time-to-live 6)
        #\O]
       ;; medium bubbles
       [(>= time-to-live 2)
        #\o]
       [else #\.]))
    (define raise-time?
      (eqv? time-till-raise 0))
    (methods
     ((tick)
      (cond
       [raise-time?
        (let ((new-y (1+ y)))
          (if (eqv? new-y bubble-lifetime)
              ;; o/~ I tried so hard... and went so far... o/~
              ($ ticky 'die)
              ;; Time to move and adjust
              (bcom (update (max 0        ; drift, but stay within confines
                                 (min (+ x drift)
                                      (1- cauldron-width))) 
                            new-y         ; move up
                            raise-delay   ; reset
                            (modify-drift drift)))))]
       ;; stay the same..
       [else
        (bcom (update x y (1- time-till-raise) drift))]))
     ((posinfo)
      (list x y bubble-shape)))))

(define (^cauldron bcom)
  ;; (define bubble-display-key
  ;;   ($ env 'new-key #;'bubble-display))
  ;; (define bubble-canvas
  ;;   (raart:blank cauldron-width bubble-max-height))
  (define ticker (spawn-ticker))

  (define (new-bubble-cooldown)
    (randrange 15 40))
  
  (let next ((bubble-cooldown (new-bubble-cooldown)))
    (define bubble-time? (eqv? bubble-cooldown 0))
    (methods
     ((tick)
      ($ ticker 'tick 'tick)
      (when bubble-time?
        ($ ticker 'to-tick
           (lambda (ticky)
             (spawn ^bubble ticky))))
      ;; (define (do-display)
      ;;   (define all-bubbles
      ;;     ($ env 'read bubble-display-key))
      ;;   (define bubbled-canvas
      ;;     (for/fold ([canvas bubble-canvas])
      ;;               ([bubble-info all-bubbles])
      ;;               (match bubble-info
      ;;                 [(list col row char)
      ;;                  (raart:place-at canvas
      ;;                                  (1- (- bubble-max-height row))
      ;;                                  col (raart:char char))])))
      ;;   (raart:vappend
      ;;    #:halign 'center
      ;;    ;; green
      ;;    (raart:fg 'green
      ;;              bubbled-canvas)
      ;;    ;; yellow
      ;;    (raart:fg 'yellow
      ;;              cauldron-raart)))
      ;; ($ display-cell do-display)
      (bcom (next (if bubble-time?
                      (new-bubble-cooldown)
                      (1- bubble-cooldown)))))
     ((render)
      ;; draw ourselves
      (define cauldron-scr 
        (make-string-drawing-screen cauldron-drawing 0 bubble-max-height))
      (define bubble-scr
        (newwin bubble-max-height cauldron-width 0 0))
      (refresh cauldron-scr)
      ;; draw bubbles
      (for-each
       (lambda (bubble)
         (match ($ bubble 'posinfo)
           ((x y bubble-shape)
            (addch %stdscr (color %GREEN-N (bold bubble-shape))
                   #:x x
                   #:y y))))
       ($ ticker 'get-ticked))
      (refresh bubble-scr)
      ))))

(define %am (make-actormap))

(define cauldron (actormap-spawn! %am ^cauldron))

;; (define %game
;;   (actormap-spawn! %am ^game))

;; read delay
(timeout! %stdscr (truncate-quotient 1000 30))

(define last-input #f)

(define (poll-for-input)
  (define input (getch %stdscr))
  (when input
    (set! last-input input))
  input)

(define update-time 0)

(define (process-input input)
  (addstr %stdscr (format #f "NEW INPUT: ~a       " input) #:x 0 #:y 2)
  'TODO)

(define (render)
  ;; (erase %stdscr)
  ($ cauldron 'render))

(define (do-update)
  ; (addstr %stdscr (format #f "UPDATE: ~a       " update-time) #:x 0 #:y 1)
  (if (= update-time 30)
      (set! update-time 0)
      (set! update-time (1+ update-time)))
  (actormap-run!
   %am
   (lambda ()
     ($ cauldron 'tick)
     (render))))

(define (add-usecs t1 add-usecs)
  (define t1-secs (car t1))
  (define t1-usecs (cdr t1))
  (define usecs-combined (+ t1-usecs add-usecs))
  (define-values (add-secs new-usecs)
    (truncate/ usecs-combined 1000000))
  (values (+ t1-secs add-secs)
          new-usecs))

(define (main)
  ;; TODO: use this
  (define tick-usecs
    (truncate-quotient 1000000 30))
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (define running? #t)
      (while running?
        ; (pk 'loop)
        ;; a really bad game loop
        (match (poll-for-input)
          ;; no input, which means it's time for an update
          (#f
           (poll-coop-repl-server %repl)
           (do-update))
          (#\q (set! running? #f))
          (input
           (process-input input)))))
    (lambda ()
      (endwin))))

(main)

