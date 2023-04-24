;;; Copyright 2022-2023 David Thompson
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

(define-module (goblins repl)
  #:use-module (system base compile)
  #:use-module (system base language)
  #:use-module (system repl common)
  #:use-module (system repl command)
  #:use-module (system repl debug)
  #:use-module (system repl repl)
  #:use-module (system vm loader)
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins utils graphviz)
  #:use-module (goblins utils random-name)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11))

;; Special exception type that REPL commands will catch in order to
;; print out friendly error messages.
(define &goblins-repl-error
  (make-exception-type '&goblins-repl-error &error '()))

(define make-goblins-repl-error (record-constructor &goblins-repl-error))

(define (repl-error message)
  (raise-exception
   (make-exception (make-goblins-repl-error)
                   (make-exception-with-message message))))

(define (call-with-goblins-error-messages thunk)
  (with-exception-handler (lambda (e)
                            (display (exception-message e))
                            (newline))
    thunk
    #:unwind? #t
    #:unwind-for-type &goblins-repl-error))

(define-syntax-rule (with-goblins-error-messages body ...)
  (call-with-goblins-error-messages (lambda () body ...)))

;; This type stores a vat event trace (as a vector rather than a list)
;; and an index into that vector, for the purpose of moving up/down
;; the trace like we're used to with stack frames.
(define-record-type <vat-debug>
  (make-vat-debug trace index)
  vat-debug?
  (trace vat-debug-trace)
  (index vat-debug-index set-vat-debug-index!))

(define (vat-debug-max-index debug)
  (- (vector-length (vat-debug-trace debug)) 1))

(define (vat-debug-bottom? debug)
  (= (vat-debug-index debug) 0))

(define (vat-debug-top? debug)
  (= (vat-debug-index debug)
     (vat-debug-max-index debug)))

(define (vat-debug-current-event debug)
  (vector-ref (vat-debug-trace debug) (vat-debug-index debug)))

(define (vat-debug-up! debug)
  (set-vat-debug-index! debug
                        (min (+ (vat-debug-index debug) 1)
                             (vat-debug-max-index debug))))

(define (vat-debug-down! debug)
  (set-vat-debug-index! debug (max (- (vat-debug-index debug) 1) 0)))

(define (vat-debug-top! debug)
  (set-vat-debug-index! debug (- (vector-length (vat-debug-trace debug)) 1)))

(define (vat-debug-bottom! debug)
  (set-vat-debug-index! debug 0))

(define (vat-debug-jump! debug timestamp)
  ;; Iterate over the trace vector, looking for an event matching
  ;; timestamp.  Traces aren't usually very long, so a linear time
  ;; search is fine.  It should be possibly to binary search, though,
  ;; due to the nature of Lamport timestamps.
  ;;
  ;; Return #t if an event with the given timestamp was found, or #f
  ;; otherwise.
  (let* ((trace (vat-debug-trace debug))
         (n (vector-length trace)))
    (let loop ((i 0))
      (and (< i n)
           (let ((event (vector-ref trace i)))
             (if (= (vat-event-timestamp event) timestamp)
                 (begin
                   (set-vat-debug-index! debug i)
                   #t)
                 (loop (+ i 1))))))))

(define current-vat-debug (make-parameter #f))

(define (current-vat-debug*)
  (let ((debug (current-vat-debug)))
    (if (vat-debug? debug)
        debug
        (repl-error "Not currently debugging a vat error."))))

;; This code is based on error-string in (system repl
;; exception-handling) and adapted to work with Guile's new exception
;; objects.
(define (error-message stack e)
  (let ((key (exception-kind e))
        (args (exception-args e)))
    (call-with-output-string
      (lambda (port)
        (let ((frame (and (< 0 (vector-length stack)) (vector-ref stack 0))))
          (print-exception port frame key args))))))

;; Our Goblins REPLs *have to be* interpreted otherwise our special
;; eval procedure won't be used and that's what makes the whole thing
;; work.
(define* (start-interpreted-repl language #:key debug)
  (let ((repl (make-repl language debug)))
    (repl-option-set! repl 'interp #t)
    (run-repl repl)
    ;; The previous procedure returns the empty list, which would get
    ;; printed as a return value when the sub-REPL is exited.  That's
    ;; a bit weird, so force the return value to be unspecified
    ;; instead.
    *unspecified*))

(define (stack->vector stack)
  (let ((v (make-vector (stack-length stack))))
    (let loop ((i 0))
      (when (< i (stack-length stack))
        (vector-set! v i (stack-ref stack i))
        (loop (+ i 1))))
    v))

(define (enter-debugger language e)
  (cond
   ;; For exceptions, launch Guile's debug sub-REPL so that both the
   ;; stack trace and the vat trace can be debugged.
   ((exception? e)
    (let* ((stack (stack->vector (actormap-turn-error-stack e)))
           (msg (error-message stack e))
           (event (vat-turn-error-event e))
           (trace (list->vector (vat-event-trace event)))
           (debug (make-debug stack 0 msg)))
      (parameterize ((current-vat-debug (make-vat-debug trace 0)))
        ;; Mimicking Guile's debugger welcome message because starting a
        ;; debug REPL doesn't do it!
        (format #t "~a\n" msg)
        (format #t "Entering a new prompt. ")
        (format #t "Type `,bt' for a backtrace or `,q' to continue.\n")
        (start-interpreted-repl language #:debug debug))))
   ;; For events with no exception, we still want to make a sub-REPL
   ;; so the vat trace can be inspected for logic errors that did not
   ;; trigger exceptions.  The Guile stack debugging tools won't be
   ;; available since there's no stack to debug.
   ((vat-event? e)
    (let ((trace (list->vector (vat-event-trace e))))
      (parameterize ((current-vat-debug (make-vat-debug trace 0)))
        (format #t "Entering a new prompt. Type `,q' to exit.\n")
        (start-interpreted-repl language))))
   (else
    (repl-error (format #f "invalid debug context: ~a" e)))))

(define (call-with-goblins-debugger language thunk)
  (with-exception-handler (lambda (e) (enter-debugger language e))
    thunk
    #:unwind? #t
    #:unwind-for-type &vat-turn-error))

;; We make a language object per-vat so that we can evaluate
;; expressions in the context of a specific vat without having to
;; introduce dynamic scoping via parameters.
(define* (make-goblins-language vat)
  (define scheme (lookup-language 'scheme))
  (define (vat-eval exp env)
    ;; Compile the expression to bytecode and load it into a thunk
    ;; that we can pass to the vat.
    (define compiled-thunk
      (load-thunk-from-memory
       (compile exp #:to 'bytecode #:env env)))
    (call-with-goblins-debugger
     goblins-language
     (lambda ()
       (call-with-vat vat compiled-thunk))))
  ;; The Goblins language is just Scheme with a special evaluator that
  ;; does vat magic.
  (define goblins-language
    (make-language #:name (format #f "goblins/~a"
                                  (or (vat-name vat) (vat-id vat)))
                   #:title "Goblins"
                   #:reader (language-reader scheme)
                   #:compilers (language-compilers scheme)
                   #:decompilers (language-decompilers scheme)
                   #:evaluator vat-eval
                   #:printer (language-printer scheme)
                   #:make-default-environment
                   (language-make-default-environment scheme)))
  goblins-language)

(define current-vat (make-parameter #f))

(define (current-vat*)
  (let ((vat (current-vat)))
    (if (vat? vat)
        vat
        (repl-error "Not in a vat.  Use ,enter-vat first."))))

(define-meta-command ((vats goblins) repl)
  "vats
Display a list of vats."
  (match (sort (all-vats)
               (lambda (a b)
                 (< (vat-id a) (vat-id b))))
    (()
     (format #t "No vats.\n"))
    (vats
     (format #t "id\tstatus\tlogging\tname\n")
     (format #t "--\t------\t-------\t----\n")
     (for-each (lambda (vat)
                 (let ((id (vat-id vat))
                       (name (or (vat-name vat) ""))
                       (status (if (vat-running? vat) "running" "stopped"))
                       (logging (if (vat-logging? vat) "enabled" "disabled")))
                   (format #t "~a\t~a\t~a\t~a\n"
                           id status logging name)))
               vats))))

(define (maybe-lookup-vat x)
  (if (vat? x) x (lookup-vat x)))

(define-meta-command ((enter-vat goblins) repl exp)
  "enter-vat vat
Enter a sub-REPL where all expressions are evaluated within VAT."
  (with-goblins-error-messages
   (let ((vat (maybe-lookup-vat (repl-eval repl exp))))
     (if (vat? vat)
         (parameterize ((current-vat vat))
           (format #t "Entering vat '~a'.  Type ',q' to exit.  Type ',help goblins' for help.\n"
                   (or (vat-name vat) (vat-id vat)))
           (start-interpreted-repl
            (make-goblins-language vat)))
         (repl-error (format #f "Not a vat: ~s" vat))))))

(define-meta-command ((vat-log-enable goblins) repl)
  "vat-log-enable
Enable vat event logging for the current vat."
  (with-goblins-error-messages
   (set-vat-logging! (current-vat*) #t)
   (display "Logging enabled.\n")))

(define-meta-command ((vat-log-disable goblins) repl)
  "vat-log-disable
Disable vat event logging for the current vat."
  (with-goblins-error-messages
   (set-vat-logging! (current-vat*) #f)
   (display "Logging disabled.\n")))

(define (check-logging-status vat)
  (unless (vat-logging? vat)
    (display "warning: Logging is disabled.  Use ,vat-log-enable to begin logging.\n")))

;; Symbolic representation of a vat event for the purpose of printing.
(define (symbolic-event event)
  (let ((msg (vat-event-message event)))
    (cond
     ((message? msg)
      `(message ,(message-to msg) ,@(message-args msg)))
     ((listen-request? msg)
      `(listen ,(message-or-request-to msg)))
     ((questioned? msg)
      `(question ,(message-or-request-to msg)))
     (else
      (repl-error (format #f "unknown message: ~a" msg))))))

(define-meta-command ((vat-tail goblins) repl #:optional (n 10))
  "vat-tail [N]
Display the most recent N messages in the current vat."
  (with-goblins-error-messages
   (let* ((vat (current-vat*))
          (len (vat-log-length vat)))
     (check-logging-status vat)
     (let loop ((i (max (- len n) 0))
                (prev-churn #f))
       (unless (= i len)
         (let* ((event (vat-log-ref vat i))
                (churn (vat-event-churn event)))
           (unless (eq? churn prev-churn)
             (format #t "Churn ~a:\n" churn))
           (format #t "  ~a: ~s\n"
                   (vat-event-timestamp event)
                   (cons (vat-event-type event)
                         (symbolic-event event)))
           (loop (+ i 1) churn)))))))

(define (vat-log-ref-by-time* vat timestamp)
  (check-logging-status vat)
  (let ((event (vat-log-ref-by-time vat timestamp)))
    (if (vat-event? event)
        event
        (repl-error
         (format #f "No event for logical timestamp ~a." timestamp)))))

(define-meta-command ((vat-trace goblins) repl #:optional timestamp)
  "vat-trace [TIMESTAMP]
Display a backtrace of events starting from TIMESTAMP in the current vat."
  (define (print-churn-id event)
    (format #t "  Churn ~a:\n" (vat-event-churn event)))
  (define (print-event event prev-event)
    (let ((vat-connector (vat-event-connector event))
          (prev-vat-connector (and prev-event (vat-event-connector prev-event)))
          (prev-churn (and prev-event (vat-event-churn prev-event))))
      (cond
       ((not (eq? vat-connector prev-vat-connector))
        (format #t "In vat ~a:\n" (vat-connector 'name))
        (print-churn-id event))
       ((not (= (vat-event-churn event) prev-churn))
        (print-churn-id event)))
      (format #t "    ~a: ~s\n"
              (vat-event-timestamp event)
              (symbolic-event event))))
  (with-goblins-error-messages
   (let* ((vat (current-vat*))
          (debug (current-vat-debug))
          (trace (cond
                  ;; User provided a timestamp.
                  ((number? timestamp)
                   (vat-event-trace
                    (vat-log-ref-by-time* vat timestamp)))
                  ;; No timestamp provided, but we are in a debugger,
                  ;; so use the current debugging trace narrowed to
                  ;; the current debug index.
                  (debug
                   (drop (vector->list (vat-debug-trace debug))
                         (vat-debug-index debug)))
                  ;; No timestamp provided and we are not in a
                  ;; debugger, use the current vat timestamp.
                  (else
                   (vat-event-trace
                    (vat-log-ref-by-time* vat (vat-clock vat)))))))
     (let loop ((events (reverse trace))
                (prev-event #f))
       (match events
         (() *unspecified*)
         ;; Collapse cross-vat send+receive events into a single frame
         ;; of the trace.  The send event gets rendered but not the
         ;; redundant receive event.
         (((? vat-send-event? event) _ . rest)
          (print-event event prev-event)
          (loop rest event))
         ((event . rest)
          (print-event event prev-event)
          (loop rest event)))))))

;; The following series of procedures are for processing vat event
;; trees, and in particular removing details of internal machinery
;; such as promise resolution.  Our current strategy relies on
;; unsatisfying heuristics that make educated guesses based on actor
;; debug names and message argument patterns.  In the future we would
;; like to enhance the information we store about actors so that we
;; can know *for sure* that a message is, say, for a promise resolver
;; and not just something that *looks like* one.
(define (fulfill-event-for? obj debug-name)
  (and (vat-receive-event? obj)
       (vat-event-local? obj)
       (vat-event-message? obj)
       (let ((msg (vat-event-message obj)))
         (and (eq? (local-object-refr-debug-name (message-to msg)) debug-name)
              ;; Look for the proper argument form to make this
              ;; heuristic less prone to false positives.
              (match (message-args msg)
                (('fulfill _) #t)
                (_ #f))))))

(define (resolver-fulfill-event? obj)
  ;; Promise resolver actors use the "^resolver" debug name.
  (fulfill-event-for? obj '^resolver))

(define (on-listener-fulfill-event? obj)
  ;; Listener actors use the "^on-listener" debug name.
  (fulfill-event-for? obj '^on-listener))

(define (promise-handler-event? obj handler-debug-name)
  (and (vat-receive-event? obj)
       (vat-event-local? obj)
       (vat-event-message? obj)
       (eq? (local-object-refr-debug-name
             (message-to (vat-event-message obj)))
            handler-debug-name)))

(define (fulfilled-handler-event? obj)
  (promise-handler-event? obj 'fulfilled-handler))

(define (finally-handler-event? obj)
  (promise-handler-event? obj 'finally-handler))

(define (collapse-promise-resolutions tree)
  ;; Collapse a ^resolver fulfill -> ^on-listener fulfill ->
  ;; fulfilled-handler sequence into just a ^resolver fulfill, for
  ;; brevity's sake.
  (vat-event-tree-map (match-lambda
                        ;; Promise resolution sequence ending in a
                        ;; leaf node.
                        (((? resolver-fulfill-event? event)
                          ((? on-listener-fulfill-event?)
                           (? fulfilled-handler-event?)))
                         event)
                        (((? resolver-fulfill-event? event)
                          ((? on-listener-fulfill-event?)
                           (? fulfilled-handler-event?)
                           (? finally-handler-event?)))
                         event)
                        ;; Promise resolution sequence with a subtree.
                        (((? resolver-fulfill-event? event)
                          ((? on-listener-fulfill-event?)
                           ((? fulfilled-handler-event?) . rest-tree)))
                         (cons event rest-tree))
                        (other other))
                      tree))

(define (remove-send-events tree)
  ;; Remove the send side of a send-receive tree since the event
  ;; messages are duplicated between them.  This removes a layer of
  ;; nesting for each cross-vat event sequence in the tree.
  (vat-event-tree-map (match-lambda
                        ((root children ...)
                         (let-values (((send-trees other-trees)
                                       (partition (match-lambda
                                                    (((? vat-send-event?) _ ...)
                                                     #t)
                                                    (_ #f))
                                                  children)))
                           (match send-trees
                             ;; No send event trees so there's nothing
                             ;; to do.
                             (()
                              (cons root children))
                             ;; Remove the send events (at the head of
                             ;; each list) and splice their subtrees
                             ;; into their parent tree.
                             (((_ send-subtrees) ...)
                              (cons root (append other-trees send-subtrees))))))
                        ;; Leaf nodes are unchanged.
                        (leaf leaf))
                      tree))

(define (remove-listen-events tree)
  (vat-event-tree-remove (match-lambda
                           ((? vat-event? event)
                            (vat-event-listen? event))
                           (_ #f))
                         tree))

(define (remove-resolver-fulfill-events tree)
  ;; Rewrite subtrees to remove leaf resolver fulfill events.  A
  ;; subtree might only have resolver fulfill events as children, in
  ;; which case it becomes eligible for removal when its parent tree
  ;; is mapped.
  (vat-event-tree-map (match-lambda
                        ((? vat-event? leaf)
                         leaf)
                        (subtree
                         (vat-event-tree-remove resolver-fulfill-event? subtree)))
                      tree))

(define (remove-system-events tree)
  (collapse-promise-resolutions
   (remove-resolver-fulfill-events
    (remove-listen-events tree))))

(define-meta-command ((vat-tree goblins) repl #:optional timestamp #:key full?)
  "vat-tree [TIMESTAMP] [#:FULL?]
Display a tree view of events starting at TIMESTAMP in the current vat."
  (define (print-branches levels)
    (match levels
      (() #t)
      ((branch?)
       (display (if branch? "├▸ " "└▸ ")))
      ((branch? . rest)
       (display (if branch? "│  " "   "))
       (print-branches rest))))
  (define (print-event event levels)
    (let* ((type (vat-event-type event))
           (msg (vat-event-message event))
           (to (message-or-request-to msg))
           (vat-connector (vat-event-connector event)))
      (print-branches levels)
      (format #t "Vat ~a, ~a: ~s\n"
              (vat-connector 'name)
              (vat-event-timestamp event)
              (symbolic-event event))))
  (define (print-list events levels)
    (match events
      ((event)
       (print-tree event (append levels (list #f))))
      ((event . rest)
       (print-tree event (append levels (list #t)))
       (print-list rest levels))))
  (define (print-tree tree levels)
    (match tree
      ((event children ..1)
       (print-event event levels)
       (print-list children levels))
      (event
       (print-event event levels))))
  (with-goblins-error-messages
   (let* ((vat (current-vat*))
          (event (vat-log-ref-by-time* vat (or timestamp (vat-clock vat))))
          (tree (vat-event-tree event)))
     (print-tree (if full?
                     tree
                     (remove-send-events (remove-system-events tree)))
                 '()))))

;; This monster procedure converts a vat timeline into a Lamport
;; causality diagram in Graphviz format.
(define (lamport-graph timeline)
  ;; Generate a list of all timestamps across all vats in the
  ;; timeline.  Duplicate values are OK.
  (define all-timestamps
    (hash-fold (lambda (_vat-connector events result)
                 (hash-fold (lambda (timestamp _event result)
                              (cons timestamp result))
                            result events))
               '() timeline))
  ;; Get the min and max timestamp values.  These will serve as the
  ;; start and end points for the timeline.  The bigger this range is,
  ;; the taller the resulting graph will be when rendered.
  (define max-t (reduce max 0 all-timestamps))
  (define min-t (reduce min max-t all-timestamps))
  ;; Generate a hash of timestamps with no events on any vat timeline.
  ;; We will skip generating nodes for these to reduce wasted vertical
  ;; space in the rendered graph.
  (define empty-timestamps
    (let ((table (make-hash-table))
          (vat-events (hash-map->list (lambda (_vat-connector events)
                                        events)
                                      timeline)))
      ;; Loop from min-t to max-t.
      (do ((t min-t (+ t 1)))
          ((> t max-t))
        ;; Does any vat timeline have an event at this timestamp?  If
        ;; not, add it to the empty timestamps hash.
        (unless (any (lambda (timeline)
                       (hashv-ref timeline t))
                     vat-events)
          (hashv-set! table t #t)))
      table))
  (define (vat-name-string vat-connector)
    (format #f "~a" (vat-connector 'name)))
  ;; Helpers for generating graph node names.
  (define (vat-node-name vat-name)
    (format #f "vat_~a" vat-name))
  (define (msg-node-name vat-name t)
    (format #f "msg_~a_~a" vat-name t))
  (define timeline-edge-color "slategray")
  (define (vat-root-node vat-name)
    `(node ,(vat-node-name vat-name)
           (@ (group ,vat-name)
              (shape "point")
              (color ,timeline-edge-color)
              (height "0.05"))))
  ;; Generate a string that describes an event.  For use as node
  ;; labels.
  (define (event-label event)
    (let ((msg (vat-event-message event)))
      (format #f "~s"
              ;; For brevity, replace the full printed output of local
              ;; object refrs with just their debug name.
              (map (match-lambda
                     ((? local-object-refr? refr)
                      (local-object-refr-debug-name refr))
                     (x x))
                   ;; Tag the different event message types.
                   (cond
                    ((message? msg)
                     `(msg ,(message-to msg) ,@(message-args msg)))
                    ((listen-request? msg)
                     `(listen ,(message-or-request-to msg)))
                    ((questioned? msg)
                     `(question ,(message-or-request-to msg)))
                    (else
                     (error "unknown message" msg)))))))
  ;; Node constructor for timestamps with an event.
  (define (event-node event vat-name t)
    `(node ,(msg-node-name vat-name t)
           (@ (label ,(event-label event))
              (group ,vat-name))))
  ;; Node constructor for timestamps where nothing happened.
  (define (empty-node vat-name t)
    `(node ,(msg-node-name vat-name t)
           (@ (group ,vat-name)
              (shape "point")
              (color ,timeline-edge-color)
              (height "0.05"))))
  ;; Helper to extract the name of a node.
  (define (node-name node)
    (match node
      (('node name _ ...) name)))
  ;; Edge constructor for two nodes on the same timeline that shows
  ;; the progression of time.
  (define (timeline-edge from to)
    `(edge ,(node-name from) ,(node-name to)))
  ;; Edge constructor for two nodes that are in different vats.  This
  ;; is the edge type used to represent cross-vat messaging.
  (define (cross-vat-edge vat-connector timestamp target)
    `(edge ,target
           ,(msg-node-name (vat-connector 'name) timestamp)
           (@ (constraint "false")
              (arrowhead "normal")
              (arrowsize "0.75")
              (color "lightcoral"))))
  ;; Starting from time 't', find the most recent event associated
  ;; with a churn and return its timestamp, skipping over empty spaces
  ;; in the timeline as necessary.  Used to form a time range that is
  ;; encapsulated in a churn sub-graph.
  (define (max-churn-timestamp t max-t last-known-t events churn)
    (if (<= t max-t)
        (match (hashv-ref events t)
          (#f
           (max-churn-timestamp (+ t 1) max-t last-known-t events churn))
          ((event . _)
           (let ((churn* (vat-event-churn event)))
             (if (= churn churn*)
                 (max-churn-timestamp (+ t 1) max-t (vat-event-timestamp event)
                                      events churn)
                 last-known-t))))
        last-known-t))
  ;; Constructor for a sub-graph containing the events of a single vat
  ;; churn.  Generates a portion of a vat timeline from 'start-t' to
  ;; 'end-t'.
  (define (churn-graph vat-connector events start-t end-t prev-node churn)
    (let* ((vat-name (vat-name-string vat-connector))
           (cluster-name (format #f "cluster_churn_~a_~a" vat-name churn)))
      (let loop ((t start-t)
                 (prev-node prev-node)
                 (nodes '())
                 (edges '()))
        ;; Timeline events are lists containing the event *and* any
        ;; connections to other events in different vats.  These
        ;; cross-vat sends will be represented as edges in the graph.
        (match-let (((event event-edges ...)
                     ;; Fetch the event for this timestamp.  If there
                     ;; isn't one, default to a structure that still
                     ;; satisfies the pattern matcher.
                     (hashv-ref events t '(#f))))
          (cond
           ;; Timestamp is a member of the empty timestamps list, do
           ;; not generate a node for it and move on to the next
           ;; timestamp.
           ((and (<= t end-t) (hashv-ref empty-timestamps t))
            (loop (+ t 1) prev-node nodes edges))
           ;; Timestamp is within the churn we are graphing, so lookup
           ;; the associated event and add a node to the graph.
           ((<= t end-t)
            (let* ((node (if event
                             (event-node event vat-name t)
                             (empty-node vat-name t)))
                   (near-edge (timeline-edge prev-node node))
                   ;; Each edge is a 2 element list containing the far
                   ;; vat connector and the far timestamp for the
                   ;; associated receive event.
                   (far-edges (map (match-lambda
                                     ((far-vat-connector far-timestamp)
                                      (cross-vat-edge far-vat-connector
                                                      far-timestamp
                                                      (node-name node))))
                                   event-edges)))
              (loop (+ t 1)
                    node
                    (cons node nodes)
                    (cons near-edge (append far-edges edges)))))
           ;; End of the churn. Return the sub-graph, the node edges,
           ;; and the final node in the churn.
           (else
            (values `(subgraph ,cluster-name
                               (attr graph (@ (label "") ; remove default label
                                              (color "gray")))
                               ,@nodes)
                    edges
                    prev-node)))))))
  ;; Constructor for a sub-graph that contains an entire vat timeline.
  ;; This graph is further broken into sub-graphs for each churn.
  ;; Timestamps with no event are excluded from churn sub-graphs.
  (define (vat-graph vat-connector events min-t max-t)
    (let* ((vat-name (vat-name-string vat-connector))
           (root-node (vat-root-node vat-name)))
      (let loop ((t min-t)
                 (prev-node root-node)
                 (nodes (list root-node))
                 (edges '()))
        (cond
         ;; Timestamp is a member of the empty timestamps list, do not
         ;; generate a node for it and move on to the next timestamp.
         ((and (<= t max-t) (hashv-ref empty-timestamps t))
          (loop (+ t 1) prev-node nodes edges))
         ;; Timestamp is within the range we are graphing, so lookup
         ;; the associated event.  If there is no event, add an empty
         ;; node.  If there is an event, add a sub-graph containing
         ;; the entire churn associated with that event.
         ((<= t max-t)
          (match (hashv-ref events t)
            ;; No event for this timestamp, make an empty node and
            ;; proceed to the next timestamp.
            (#f
             (let* ((node (empty-node vat-name t))
                    (edge (timeline-edge prev-node node)))
               (loop (+ t 1) node (cons node nodes) (cons edge edges))))
            ;; There is an event, generate a sub-graph for the churn,
            ;; add the sub-graph and all associated edges, and then
            ;; proceed to the timestamp after the end of the churn.
            ((event . _)
             (let* ((churn (vat-event-churn event))
                    (end-t (max-churn-timestamp t max-t t events churn)))
               (let-values (((subgraph new-edges final-node)
                             (churn-graph vat-connector events t end-t
                                          prev-node churn)))
                 (loop (+ end-t 1) final-node (cons subgraph nodes)
                       (append new-edges edges)))))))
         ;; We've reached the end of the timeline, so return the
         ;; sub-graph for the vat.
         (else
          (let* ((end-t (+ max-t 1))
                 (end-node-name (msg-node-name vat-name end-t)))
            (cons `(subgraph ,(format #f "cluster_vat_~a" vat-name)
                             (attr graph (@ (label ,vat-name)
                                            (style "solid")
                                            (color "transparent")
                                            (fontsize "14")
                                            (nodesep "0.02")
                                            (margin "2.0")))
                             ,@(cons `(node ,end-node-name
                                            (@ (style "invis")))
                                     nodes))
                  (cons `(edge ,(node-name prev-node)
                               ,end-node-name
                               (@ (arrowhead "normal")))
                        edges))))))))
  ;; Build a directed graph with a sub-graph per vat timeline.
  `(digraph goblins
            (attr graph (@ (ordering "out")
                           (fontsize "8")
                           (ranksep "0.1")
                           (splines "line")))
            (attr edge (@ (arrowhead "none")
                          (fontsize "8")
                          (color ,timeline-edge-color)))
            (attr node (@ (shape "box")
                          (style "filled,rounded")
                          (color "lightskyblue")
                          (fontsize "10")
                          (height "0.1")
                          (margin "0.01")))
            ,@(hash-fold (lambda (vat-connector events result)
                           (append (vat-graph vat-connector events min-t max-t)
                                   result))
                         '()
                         timeline)))

(define-meta-command ((vat-graph goblins) repl #:optional timestamp #:key full?)
  "vat-graph [TIMESTAMP]
Generate an image of the event tree for TIMESTAMP in the current vat."
  (with-goblins-error-messages
   (let* ((vat (current-vat*))
          (event (vat-log-ref-by-time* vat (or timestamp (vat-clock vat))))
          (tree (vat-event-tree event))
          (timeline (vat-event-tree->timeline
                     (if full?
                         tree
                         (remove-system-events tree))))
          (tmpdir (string-append (or (getenv "TMPDIR") "/tmp")
                                 "/goblins-repl-images"))
          ;; Generate a reasonably random file name.  Geiser caches
          ;; images based on file name so if we used the same name all
          ;; the time things would look broken in the REPL even though
          ;; the file is actually being updated in the file system.
          (base-file-name (random-name 16))
          (dot-file-name (string-append tmpdir "/" base-file-name ".dot"))
          (png-file-name (string-append tmpdir "/" base-file-name ".png")))
     ;; Create directory, if necessary.
     (unless (file-exists? tmpdir)
       (mkdir tmpdir))
     ;; Save graph to file using Graphviz DOT format.
     (call-with-output-file dot-file-name
       (lambda (port)
         (sdot->dot (lamport-graph timeline) port)))
     ;; Render the graph to an image file.
     (if (zero? (run-dot "-T" "png" "-o" png-file-name dot-file-name))
         ;; Print in this specific format that Geiser understands so
         ;; it will substitute the text for the actual image.  Cool
         ;; beans!
         (format #t "#<Image: ~a>\n" png-file-name)
         (display "failed to generate graph\n")))))

(define-meta-command ((vat-resolve goblins) repl exp)
  "vat-resolve EXP
Wait for the promise returned by EXP to resolve and print the result."
  (with-goblins-error-messages
   (let* ((vat (current-vat*)))
     ;; Evaluate the expression and make sure it's a promise before
     ;; going any further.
     (match (repl-eval repl exp)
       ((? promise-refr? promise)
        (let ((done? (make-condition))
              (result (make-atomic-box #f)))
          (match (sigaction SIGINT)
            ((prev-sigint-handler . prev-sigint-flags)
             ;; Catch SIGINT and stop waiting for the promise to resolve.
             ;; Nothing will be printed as a result.
             (sigaction SIGINT
               (lambda _
                 (signal-condition! done?)))
             ;; Wait for promise to resolve within the vat and update the
             ;; 'result' box with the results.
             (with-vat vat
               (on promise
                   (lambda (val)
                     (atomic-box-set! result `#(fulfilled ,val)))
                   #:catch
                   (lambda (exception)
                     (atomic-box-set! result `#(broken ,exception)))
                   #:finally
                   (lambda ()
                     ;; Tell the REPL thread that it can print the result
                     ;; now.
                     (signal-condition! done?))))
             ;; Wait until the promise is fulfilled or broken.
             (perform-operation (wait-operation done?))
             ;; Restore original SIGINT handler.
             (sigaction SIGINT prev-sigint-handler prev-sigint-flags)))
          (match (atomic-box-ref result)
            ;; Either the wait was terminated by SIGINT or the
            ;; response is unspecified.  Either way, don't print
            ;; anything.
            ((or #f #('fulfilled (? unspecified?)))
             #f)
            ;; Promise was fulfilled, so print value.
            (#('fulfilled val)
             (write val)
             (newline))
            ;; Promise was broken, so print exception.
            (#('broken exception)
             (display "Promise broken:\n")
             ;; We don't have the stack but we can still
             ;; print some details about the exception.
             (print-exception (current-output-port)
                              #f ; no stack :(
                              (exception-kind exception)
                              (exception-args exception))))))
       ;; Expression didn't evaluate to a promise, so tell the user
       ;; that.
       (obj (repl-error (format #f "Not a promise: ~s" obj)))))))

(define-meta-command ((vat-errors goblins) repl)
  "vat-errors
Display a list of errors that have occurred in the current vat."
  (define (print-error event exception)
    (let* ((stack (actormap-turn-error-stack exception))
           (frame (stack-ref stack 0)))
      (format #t "At churn ~a, event ~a, file ~a:\n  In procedure ~a: ~a\n"
              (vat-event-churn event)
              (vat-event-timestamp event)
              (match (frame-source frame)
                ((_ file-name line . column)
                 (format #f "~a:~a:~a"
                         (if file-name
                             (basename file-name)
                             "unknown")
                         line column))
                (_ "unknown"))
              (or (frame-procedure-name frame)
                  "unknown")
              (if (and (exception-with-message? exception)
                       (exception-with-irritants? exception))
                  (apply format #f (exception-message exception)
                         (exception-irritants exception))
                  ""))))
  (with-goblins-error-messages
   (let ((vat (current-vat*)))
     ;; Sort errors by timestamp.
     (match (sort (vat-log-errors vat)
                  (match-lambda*
                    (((a . _) (b . _))
                     (< (vat-event-timestamp a)
                        (vat-event-timestamp b)))))
       (()
        (format #t "No errors.\n"))
       (errors
        (for-each (match-lambda
                    ((event . e)
                     (print-error event e)))
                  errors))))))

(define-meta-command ((vat-debug goblins) repl timestamp)
  "vat-debug [TIMESTAMP]
Debug error associated with the event at TIMESTAMP."
  (with-goblins-error-messages
   (let* ((vat (current-vat*))
          (timestamp* (if (integer? timestamp)
                          timestamp
                          (repl-eval repl timestamp)))
          (event (vat-log-ref-by-time* vat timestamp*))
          (exception (vat-log-error-for-event vat event)))
     (enter-debugger (repl-language repl)
                     (if (exception? exception)
                         exception
                         event)))))

(define (print-current-vat-debug-event debug)
  (let ((event (vat-debug-current-event debug)))
    (format #t "Now in vat ~a, event ~a:\n  ~s\n"
            ((vat-event-connector event) 'name)
            (vat-event-timestamp event)
            (cons (vat-event-type event)
                  (symbolic-event event)))))

(define-meta-command ((vat-up goblins) repl)
  "vat-up
Move to the previous event in the current vat debug trace."
  (with-goblins-error-messages
   (let ((debug (current-vat-debug*)))
     (if (vat-debug-top? debug)
         (format #t "Already at oldest event.\n")
         (begin
           (vat-debug-up! debug)
           (print-current-vat-debug-event debug))))))

(define-meta-command ((vat-down goblins) repl)
  "vat-down
Move to the next event in the current vat debug trace."
  (with-goblins-error-messages
   (let ((debug (current-vat-debug*)))
     (if (vat-debug-bottom? debug)
         (format #t "Already at most recent event.\n")
         (begin
           (vat-debug-down! debug)
           (print-current-vat-debug-event debug))))))

(define-meta-command ((vat-top goblins) repl)
  "vat-top
Move to the oldest event in the current vat debug trace."
  (with-goblins-error-messages
   (let ((debug (current-vat-debug*)))
     (if (vat-debug-top? debug)
         (format #t "Already at oldest event.\n")
         (begin
           (vat-debug-top! debug)
           (print-current-vat-debug-event debug))))))

(define-meta-command ((vat-bottom goblins) repl)
  "vat-bottom
Move to the most recent event in the current vat debug trace."
  (with-goblins-error-messages
   (let ((debug (current-vat-debug*)))
     (if (vat-debug-bottom? debug)
         (format #t "Already at most recent event.\n")
         (begin
           (vat-debug-bottom! debug)
           (print-current-vat-debug-event debug))))))

(define-meta-command ((vat-jump goblins) repl timestamp)
  "vat-jump TIMESTAMP
Move to the event for TIMESTAMP in the current vat debug trace."
  (with-goblins-error-messages
   (let ((debug (current-vat-debug*)))
     (unless (vat-debug-jump! debug timestamp)
       (repl-error
        (format #f "no event with timestamp ~a in current trace" timestamp)))
     (print-current-vat-debug-event debug))))

(define-meta-command ((vat-peek goblins) repl refr . args)
  "vat-peek REFR [ARGS ...]
Send ARGS to REFR using the snapshot for the current debugger event."
  (with-goblins-error-messages
   (let ((debug (current-vat-debug*)))
     (let ((event (vat-debug-current-event debug)))
       (format #t "~s\n"
               (apply actormap-peek (vat-event-snapshot event)
                      (repl-eval repl `(list ,refr ,@args))))))))
