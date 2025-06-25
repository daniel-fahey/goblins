;;; Copyright 2019-2021 Christine Lemmer-Webber
;;; Copyright 2023 David Thompson
;;; Copyright 2024-2025 Jessica Tallon
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

(define-module (goblins ocapn captp)
  #:use-module ((fibers) #:select (spawn-fiber))
  #:use-module ((fibers timers) #:select (sleep))
  #:use-module ((goblins core) #:hide ($))
  #:use-module ((goblins core) #:select ($) #:prefix $)
  #:use-module (goblins core-types)
  #:use-module (goblins vat)
  #:use-module (goblins utils ghash)
  #:use-module (goblins inbox)
  #:use-module (goblins abstract-types)
  #:use-module (goblins define-actor)
  #:use-module (goblins ocapn captp-types)
  #:use-module (goblins ocapn gc)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins actor-lib nonce-registry)
  #:use-module (goblins actor-lib swappable)
  #:use-module (goblins actor-lib ward)
  #:use-module (goblins utils assert-type)
  #:use-module (goblins utils simple-sealers)
  #:use-module (goblins utils bytes-stuff)
  #:use-module (goblins utils crypto)
  #:use-module (goblins contrib syrup)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (rnrs bytevectors)
  #:use-module (fibers channels)
  #:export (spawn-mycapn captp-env))

(define captp-version "goblins-0.12")

;; This should be better documented, and will when it becomes more of
;; a "standardized protocol" as opposed to a "bespoke implementation".
;;
;; Much of CapTP here based on E's docs:
;;   http://erights.org/elib/distrib/captp/index.html
;; and capnproto's writeups:
;;   https://capnproto.org/rpc.html
;;   https://github.com/sandstorm-io/capnproto/blob/master/c++/src/capnp/rpc.capnp
;;
;; For the gory details of "Chris figuring out how CapTP works"
;; see these monster threads:
;;   https://groups.google.com/g/cap-talk/c/xWv2-J62g-I
;;   https://groups.google.com/g/cap-talk/c/-JYtc-L9OvQ
;;
;; For the handoff stuff:
;;   https://dustycloud.org/tmp/captp-handoff-musings.org.txt
;;   https://dustycloud.org/misc/3vat-handoff-scaled.jpg

(define (setup-captp-conn send-to-remote
                          ;; coordinates between multiple captp connections:
                          ;; handoffs, etc.
                          coordinator
                          bootstrap-obj
                          intra-node-warden intra-node-incanter)
  ;; position sealers, so we know this really is from our imports/exports
  ;; @@: Not great protection, subject to a reuse attack, but really
  ;;   this is just an extra step... in general we shouldn't be exposing
  ;;   the refr internals to most users
  (define-values (pos-seal pos-unseal pos-sealed?)
    (make-sealer-triplet))
  (define-values (partition-seal partition-unseal partition-tm?)
    (make-sealer-triplet))

  (define (new-question-finder)
    (let ((question-finder (make-question-finder (pos-seal next-question-pos))))
      ;; Install our question at this question id.
      (hashq-set! questions question-finder next-question-pos)
      ;; Add it to the gc guardian.
      (captp-gc-register! captp-gc question-finder)
      ;; Increment the next-question id.
      (set! next-question-pos (1+ next-question-pos))
      question-finder))

  (define (_handle-message msg)
    (match msg
      [(or (? message?) (? questioned?))
       (<-np-extern internal-handler
                    (cmd-send-message msg))]
      [($ <op:abort> reason)
       (<-np-extern internal-handler
                    (internal-shutdown 'abort reason))]
      [($ <listen-request> _ to-refr listener wants-partial?)
       (<-np-extern internal-handler
                    (cmd-send-listen to-refr listener
                                     wants-partial?))])
    *unspecified*)

  (define (_partition-unsealer-tm-cons)
    (cons partition-unseal partition-tm?))

  (define* (_listen-request to-refr listen-refr
                            #:key [wants-partial? #f])
    (<-np-extern internal-handler
                 (cmd-send-listen to-refr listen-refr
                                  wants-partial?)))

  (define (^connector-obj _bcom)
    (define intra-node-beh
      (methods
       [(get-handoff-privkey)
        ($$ coordinator 'get-handoff-privkey)]
       [(get-remote-location)
        ($$ coordinator 'get-remote-location)]
       [(get-remote-bootstrap)
        remote-bootstrap-obj]
       [(get-session-name)
        ($$ coordinator 'get-session-name)]
       [(get-our-side-name)
        ($$ coordinator 'get-our-side-name)]))
    (define main-beh
      (methods
       [(resolve-on-sever sever-resolver)
        (if running?
            (let* ((noop-beh
                    (lambda () 'no-op))
                   (^cancel-sever-notification
                    (lambda (bcom)
                      (lambda ()
                        ($$ interested-in-sever 'remove sever-resolver)
                        (bcom noop-beh)))))
              ($$ interested-in-sever 'add sever-resolver)
              (spawn ^cancel-sever-notification))
            (match shutdown-reason
              [(shutdown-type reason)
               ($$ sever-resolver 'fulfill (list 'severed shutdown-type reason))]))]
       [(cancel-sever-interest sever-resolver)
        ($$ interested-in-sever 'remove sever-resolver)]))
    (ward intra-node-warden intra-node-beh
          #:extends main-beh))
  (define connector-obj (spawn ^connector-obj))
  (define (_get-connector-obj) connector-obj)

  (define (same-connection? refr)
    (and (remote-refr? refr)
         (eq? (remote-refr-captp-connector refr) captp-connector)))

  (define captp-connector
    (methods
     [handle-message _handle-message]
     [new-question-finder new-question-finder]
     [listen _listen-request]
     [partition-unsealer-tm-cons _partition-unsealer-tm-cons]
     [same-connection? same-connection?]
     ;; For all the things that we don't want thread stompiness on...
     [connector-obj _get-connector-obj]))

  (define next-export-pos 0)
  (define next-question-pos 0)
  ;; (define next-promise-pos 0)

  (define exports-val2pos (make-hash-table))    ; (eq)  exports[val]:   chosen by us
  (define exports-pos2val (make-hash-table))    ; (eqv) exports[pos]:   chosen by us
  (define imports (make-weak-value-hash-table)) ; (eqv) imports:        chosen by peer
  (define questions (make-weak-key-hash-table)) ; (eq)  questions:      chosen by us
  (define answers (make-hash-table))            ; (eqv) answers:        chosen by peer

  ;; TODO: This should really be some kind of box that the other side
  ;;   can query, right?
  (define running? #t)
  (define shutdown-reason #f)

  ;; A guardian to collect objects and question finders that are no
  ;; longer being referenced.
  (define captp-gc-ch (make-channel))
  (define captp-gc (make-captp-gc captp-gc-ch))

  ;; These are imports that we've processed when we already had allocated
  ;; a reference.  We batch send GC messages about these as appropriate.
  ;; Note that we say "spare" because there's one more count that's
  ;; associated with the reference itself.
  ;; Mapping of slot position -> count
  ;; IMPORTANT: Do not mutate this outside of the gc loop.
  (define spare-import-counts
    (make-hash-table))  ; (eqv)
  ;; The inverse: tracking how many export numbers we've given so we can
  ;; know when it hits 0 and is ok to remove
  (define export-counts
    (make-hash-table))  ; (eqv)

  (define (increment-spare-imports-count! import-pos)
    (syscaller-free-fiber
     (lambda ()
       (put-message captp-gc-ch `(increment ,import-pos)))))
  (define (decrement-exports-count-maybe-remove! export-pos delta)
    (assert-type export-pos integer?)
    (assert-type delta integer?)
    (match (hashv-ref export-counts export-pos #f)
      [(and (? integer?) (? positive? cur-count))
       (match (- cur-count delta)
         ;; time to remove
         [0
          (hashv-remove! export-counts export-pos)
          ;; Remove this export from both
          (let ([val (hashv-ref exports-pos2val export-pos)])
            (hashq-remove! exports-val2pos val)
            (hashv-remove! exports-pos2val export-pos))]
         ;; decremented but still positive
         [(and (? integer?) (? positive? new-count))
          (hashv-set! export-counts export-pos new-count)]
         [neg-count
          (error 'exports-gc-error
                 "Tried decrementing export-pos ~a by ~a but that's negative: ~a"
                 neg-count)])]
      [other-val
       (error 'exports-gc-error
              "Tried to decrement the exports count for position ~a but its value was ~a"
              export-pos other-val)]))

  (define (gc:question sealed-pos)
    (<-np-extern internal-handler (cmd-send-gc-answer (pos-unseal sealed-pos))))

  (define (gc:import sealed-pos)
    (let* ((import-pos (pos-unseal sealed-pos))
           (spare-count (or (hashv-ref spare-import-counts import-pos) 0)))
      ;; We no longer need to keep track of the spare count.
      (hashv-remove! spare-import-counts import-pos)
      (<-np-extern internal-handler
                   ;; The number of references is one more than the number of
                   ;; spares.
                   (cmd-send-gc-export import-pos (+ spare-count 1)))))

  (define (gc-loop)
    (when running?
      (match (captp-gc-get captp-gc)
        (('gc-remote-refr sealed-pos)
         (gc:import sealed-pos))
        (('gc-question sealed-pos)
         (gc:question sealed-pos))
        (('increment import-pos)
         ;; Not strictly a GC operation, but we are the only fiber allowed to
         ;; modify the spare-import-counts hashmap.
         (hashv-set! spare-import-counts import-pos
                     (1+ (hashv-ref spare-import-counts import-pos 0))))
        (err (error "Unhandled GC value" err)))
      (gc-loop)))
  (spawn-fiber gc-loop)

  ;; Possibly install an export for this local refr, and return
  ;; this export id
  ;; TODO: we maybe need to differentiate between local-live-refr and
  ;;   remote-live-proxy-refr (once we set that up)?
  (define (maybe-install-export! refr)
    (assert-type refr live-refr?)
    (cond
     ;; Already have it, no need to increment next-export-pos
     [(hashq-ref exports-val2pos refr)
      =>
      (lambda (export-pos)
        ;; However, we do need to increment our export count
        (match (hashv-ref export-counts export-pos #f)
          ;; Uh, we screwed up our bookkeeping at some point
          [#f
           (error 'no-export-count-wtf
                  "No export count for ~a" export-pos)]
          [cur-count
           (hashv-set! export-counts export-pos (1+ cur-count))])
        ;; now finally return the export position
        export-pos)]
     ;; Nope, let's export this
     [else
      (let ((export-pos next-export-pos))
        ;; get this export-pos and increment next-export-pos
        (set! next-export-pos (1+ export-pos))
        ;; install in both export tables
        (hashv-set! exports-pos2val export-pos
                    refr)
        (hashq-set! exports-val2pos refr
                    export-pos)
        ;; (sanity check:) make sure there's no export count currently
        (when (hashv-ref export-counts export-pos)
          (error 'shouldnt-be-export-count-wtf
                 "Adding a new export but there was already an export count for pos: ~a"
                 export-pos))
        ;; and set the export count to 1
        (hashv-set! export-counts export-pos 1)
        export-pos)]))

  (define (marshall-local-refr! local-refr)
    (assert-type local-refr local-refr?)
    (let ((export-pos (maybe-install-export! local-refr)))
      (match local-refr
        [(? local-object-refr?)
         (desc:import-object export-pos)]
        [(? local-promise-refr?)
         (desc:import-promise export-pos)])))

  (define (maybe-install-import! import-desc)
    (define import-pos
      (desc:import-pos import-desc))
    (define (install-new-import!)
      ;; construct the new reference...
      (define new-refr
        (match import-desc
          [(? desc:import-object?)
           (make-remote-object-refr captp-connector
                                    (pos-seal import-pos))]
          [(? desc:import-promise?)
           (make-remote-promise-refr captp-connector
                                     (pos-seal import-pos))]))
      ;; Install it...
      (hashv-set! imports import-pos new-refr)
      ;; add to the guardian...
      (captp-gc-register! captp-gc new-refr)
      ;; and return it.
      new-refr)
    (cond
     [(hashv-ref imports import-pos)
      =>
      (lambda (import)
        ;; Oh, we've already got that.  Reference and return it.
        (match import
          ;; Possible race condition: Apparently it was GC'ed
          ;; mid-operation so now we need to add it back
          ;; @@: *sweating profusely* but is this all the possible
          ;;     race conditions???
          [#f (install-new-import!)]
          ;; looks like we got the refr, return as-is
          [refr
           (increment-spare-imports-count! import-pos)
           refr]))]
     [else
      (install-new-import!)]))

  ;; general argument marshall/unmarshall for import/export

  ;; TODO: need to handle lists/dotted-lists/vectors
  (define (outgoing-pre-marshall! obj)
    (match obj
      [() '()]
      [(_ ...) (map outgoing-pre-marshall! obj)]
      ;; Dotted lists/cons cells are not serializable by syrup, tag them.
      [(head . tail)
       (make-tagged* 'pair
                     (outgoing-pre-marshall! head)
                     (outgoing-pre-marshall! tail))]
      [(? vector?)
       (make-tagged
        'vec
        (let lp ((i 0))
          (if (= i (vector-length obj))
              '()
               (cons (outgoing-pre-marshall! (vector-ref obj i))
                     (lp (1+ i))))))]
      [(? hash-table?)
       ;; TODO: let's use "ghashes", which hash on eq? for live-refs
       ;; and on equal? for everything else
       (hash-fold
        (lambda (key val prev)
          (vhash-cons (outgoing-pre-marshall! key)
                      (outgoing-pre-marshall! val)
                      prev))
        vlist-null
        obj)]
      [(? ghash?)
       (ghash-fold
        (lambda (key value prev)
          (ghash-set prev
                     (outgoing-pre-marshall! key)
                     (outgoing-pre-marshall! value)))
        (make-ghash)
        obj)]
      [(? gset?)
       (gset-fold
        (lambda (item this-set)
          (gset-add this-set (outgoing-pre-marshall! item)))
        (make-gset)
        obj)]
      [(? local-promise-refr?)
       (desc:import-promise (maybe-install-export! obj))]
      [(? local-object-refr?)
       (desc:import-object (maybe-install-export! obj))]
      [(? remote-refr?)
       (let ((refr-captp-connector (remote-refr-captp-connector obj)))
         (cond
          ;; from this captp
          [(eq? refr-captp-connector captp-connector)
           (desc:export (pos-unseal (remote-refr-sealed-pos obj)))]
          ;; elsewhere, let the coordinator do it
          [else
           ($$ coordinator 'make-handoff-base-cert obj)]))]
      [(? unspecified?)
       (make-tagged* 'void)]
      [(? keyword?)
       (make-tagged* 'kw (keyword->symbol obj))]
      [(? error?)
       (make-tagged* 'exn:fail:mystery)]
      ;; And here's the general-purpose record that users can use
      ;; for whatever purpose is appropriate
      [($ <tagged> label data)
       (make-tagged* 'user-record label data)]
      [_ obj]))

  (define (incoming-post-unmarshall! obj)
    (match obj
      [(obj ...)
       (map incoming-post-unmarshall! obj)]
      [($ <tagged> 'pair (head tail))
       (cons head tail)]
      [($ <tagged> 'vec vec-data)
       (define vec (make-vector (length vec-data)))
       (let lp ((index 0)
                (lst vec-data))
         (match lst
           (() vec)
           ((head . tail)
            (vector-set! vec index (incoming-post-unmarshall! head))
            (lp (+ 1 index) tail))))]
      [(? hash-table?)
       (hash-fold
        (lambda (key val prev)
          (vhash-cons (incoming-post-unmarshall! key)
                      (incoming-post-unmarshall! val)
                      prev))
        vlist-null
        obj)]
      [(? ghash?)
       (ghash-fold
        (lambda (key value prev)
          (ghash-set prev
                     (incoming-post-unmarshall! key)
                     (incoming-post-unmarshall! value)))
        (make-ghash)
        obj)]
      [(? gset?)
       (gset-fold
        (lambda (item this-gset)
          (gset-add this-gset (incoming-post-unmarshall! item)))
        (make-gset)
        obj)]
      [(or (? desc:import-promise?) (? desc:import-object?))
       (maybe-install-import! obj)]
      [($ <desc:export> pos)
       (hashv-ref exports-pos2val pos)]
      [($ <tagged> 'exn:fail:mystery '())
       (make-exception
        (make-mystery-exception)
        (make-exception-with-message "Unknown error occured with remote object")
        (make-exception-with-irritants '()))]
      [($ <tagged> 'void '())
       *unspecified*]
      [($ <tagged> 'kw `(,keyword))
       (symbol->keyword keyword)]
      ;; unserialize user-defined records
      [($ <tagged> 'user-record (list label data))
       (make-tagged label data)]
      [($ <tagged> unknown-tag data)
       (error 'captp-unknown-record-rag "Unknown tag: ~a"
              unknown-tag)]
      [(? signed-handoff-give? sig-envelope-and-handoff)
       ;; We need to send this message to the coordinator, which will
       ;; work with the node to (hopefully) get it to the right
       ;; destination
       ($$ coordinator 'start-retrieve-handoff sig-envelope-and-handoff)]
      [_ obj]))

  (define (unmarshall-to-desc to-desc)
    (match to-desc
      [($ <desc:export> export-pos)
       (hashv-ref exports-pos2val export-pos)]
      [($ <desc:answer> answer-pos)
       (hashv-ref answers answer-pos)]))

  (define (marshall-to obj)
    (match obj
      [(? question-finder?)
       (desc:answer (or (hashq-ref questions obj)
                        (error "No such entry in questions" obj)))]
      [(? remote-refr?)
       (let ((refr-captp-connector
              (remote-refr-captp-connector obj)))
         (cond
          ;; from this captp
          [(eq? refr-captp-connector captp-connector)
           (desc:export (pos-unseal (remote-refr-sealed-pos obj)))]
          [else
           (error 'captp-to-wrong-node)]))]))

  (define (install-answer! answer-pos resolve-me-desc)
    (define resolve-me
      (maybe-install-import! resolve-me-desc))
    (when (hashv-ref answers answer-pos)
      (error 'already-have-answer
             "~a" answer-pos))
    (let-values (((answer-promise answer-resolver)
                 (spawn-promise-and-resolver)))
      (hashv-set! answers answer-pos answer-promise)
      (listen-to answer-promise resolve-me)
      (values answer-promise answer-resolver)))

  ;; Resolvers that are interested in when we're tearing down
  (define interested-in-sever
    (spawn ^seteq))

  (define (tear-it-down shutdown-type reason)
    (set! exports-val2pos #f)
    (set! exports-pos2val #f)
    (set! imports #f)
    (set! questions #f)
    (set! answers #f)
    (set! running? #f)
    (captp-gc-halt! captp-gc)
    (set! shutdown-reason (list shutdown-type reason))
    (for-each
     (lambda (interested)
       (<-np interested 'fulfill (list 'severed shutdown-type
                                       reason)))
     ($$ interested-in-sever 'as-list))
    (set! interested-in-sever #f))

  ;; The bootstrap on every session must be exported at position 0
  ;; Lets setup both the remote bootstrap refr for that object
  ;; and export our local one.
  (define remote-bootstrap-obj
    (maybe-install-import! (desc:import-object 0)))
  (unless (eq? (maybe-install-export! bootstrap-obj) 0)
    (error "Bootstrap object MUST be exported at position 0"))

  (define (^captp-incoming-handler bcom)
    (lambda (msg)
      (unless running?
        (error 'captp-breakage "Captp session is no longer running but got ~a"
               msg))
      (match msg
        ;; TODO: Handle case where the target doesn't exist?
        ;;   Or maybe just generally handle unmarshalling errors :P
        [($ <op:deliver-only> to-desc args-marshalled)
         (let*-values (((args)
                        (incoming-post-unmarshall! args-marshalled))
                       ((target) (unmarshall-to-desc to-desc)))
           (apply <-np target args)
           *unspecified*)]
        [($ <op:deliver> to-desc
                         args-marshalled
                         ;; answer-pos is either an integer (promise pipelining)
                         ;; or #f (no pipelining)
                         (and (or (? integer?) #f)
                              answer-pos)
                         resolve-me-desc)
         (define (do-it)
           (define args
             (incoming-post-unmarshall! args-marshalled))
           (define target (unmarshall-to-desc to-desc))
           (define sent-promise
             (apply <- target args))
           ;; We're either resolving the to the answer promise we create
           ;; or we're resolving to the actual object described by resolve-me-desc
           ;;
           ;; The former case is an indirection because messages pipelined
           ;; to a to-be-answered object are simply sent to a local promise
           ;; which will eventually resolve to the answer.
           ;;
           ;; It's possible that something more efficient could be done
           ;; than throwing in an intermediate promise pair that we inform the
           ;; other side of; maybe re-evaluate when we handle
           ;; automatic-severance-on-session-disconnect.
           (cond
            [answer-pos
             (let-values (((_answer-promise answer-resolver)
                           (install-answer! answer-pos resolve-me-desc)))
               ($$ answer-resolver 'fulfill sent-promise))]
            [else
             (let ((to-resolve
                    (maybe-install-import! resolve-me-desc)))
               (<-np to-resolve 'fulfill sent-promise))])
           *unspecified*)
         (do-it)]

        ;; TODO: Here's where we have to record that a listening interest
        ;; has occured, assuming we do the "automatically notify on session
        ;; severance" thing?
        ;;
        ;; Which means we'll also have to track incoming resolutions to
        ;; this promise somehow...?
        ;;
        ;; Actually the easiest thing to do here would be to create our own
        ;; promise-resolver pair, right here, at the captp perimeter, which
        ;; pipelines the result.
        [($ <op:listen> (and (or (? desc:export?) (? desc:answer?)) to-desc)
                        (? desc:import? listener-desc)
                        (? boolean? wants-partial?))
         (let ((to-refr
                (unmarshall-to-desc to-desc))
               (listener
                (incoming-post-unmarshall! listener-desc)))
           (listen-to to-refr listener
                      #:wants-partial? wants-partial?)
           *unspecified*)]
        [($ <op:gc-answer> answer-pos)
         (hashv-remove! answers answer-pos)]
        [($ <op:gc-export> (? integer? export-pos) (? integer? wire-delta))
         (decrement-exports-count-maybe-remove! export-pos wire-delta)]
        [($ <op:abort> (? string? reason))
         (tear-it-down 'abort reason)]
        [($ <internal-shutdown> (? symbol? type) (? string? reason))
         (when (eq? type 'abort)
           (send-to-remote (op:abort reason)))
         (tear-it-down type reason)]
        [other-message
         (error 'invalid-message "~a" other-message)])))

  (define (^internal-handler bcom)
    (lambda (cmd)
      (define (running-handle-cmd cmd)
        (match cmd
          [($ <cmd-send-message> msg)
           (define-values (real-msg answer-pos)
             (match msg
               [(? message?)
                (values msg #f)]
               [($ <questioned> msg answer-this-question)
                (values msg (hashq-ref questions answer-this-question))]))
           (match-let ((($ <message> _ to resolve-me args)
                        real-msg))
             (define deliver-msg
               (if resolve-me
                   (op:deliver (marshall-to to)
                               (outgoing-pre-marshall! args)
                               answer-pos
                               (marshall-local-refr! resolve-me))
                   (op:deliver-only (marshall-to to)
                                    (outgoing-pre-marshall! args))))
             (send-to-remote deliver-msg))]
          [($ <cmd-send-listen> (? remote-refr? to-refr) (? local-refr? listener-refr)
                                (? boolean? wants-partial?))
           (let ((listen-msg
                  (op:listen (marshall-to to-refr)
                             (outgoing-pre-marshall! listener-refr)
                             wants-partial?)))
             (send-to-remote listen-msg))]
          [($ <cmd-send-gc-answer> (? integer? answer-pos))
           (send-to-remote (op:gc-answer answer-pos))]
          [($ <cmd-send-gc-export> (? integer? export-pos) (? integer? wire-delta))
           (send-to-remote (op:gc-export export-pos wire-delta))]
          [($ <internal-shutdown> shutdown-type reason)
           (when (eq? shutdown-type 'abort)
             (send-to-remote (op:abort reason)))
           (tear-it-down shutdown-type reason)]))
      (define (broken-handle-cmd cmd)
        (match cmd
          [($ <cmd-send-message> msg)
           (define resolve-me
             (match msg
               ((? message?)
                (message-resolve-me msg))
               ))
           (match-let ((($ <message> _ to resolve-me args)
                        msg))
             (when resolve-me
               (<-np resolve-me 'break (captp-session-severed))))]
          [($ <cmd-send-listen> (? remote-refr? to-refr) (? local-refr? listener-refr)
                                (? boolean? wants-partial?))
           (<-np listener-refr 'break (captp-session-severed))]
          [($ <cmd-send-gc-answer> (? integer? answer-pos))
           'no-op]
          [($ <cmd-send-gc-export> (? integer? export-pos))
           'no-op]
          [($ <internal-shutdown> _shutdown-type _reason)
           'no-op]))
      (if running?
          (running-handle-cmd cmd)
          (broken-handle-cmd cmd))))

  (define captp-incoming-handler
    (spawn ^captp-incoming-handler))
  (define internal-handler
    (spawn ^internal-handler))

  (values captp-incoming-handler remote-bootstrap-obj))

(define* (^coordinator bcom router our-location-vow
                       intra-node-warden intra-node-incanter
                       #:key [handoff-key-pair (generate-key-pair)])
  ;; counters used to increment how many handoff requests have been
  ;; made in this session to prevent replay attacks.
  ;; every time a *request* is made, this should be incremented.
  (define our-handoff-count
    (spawn ^cell 0))
  (define remote-handoff-count
    (spawn ^cell 0))

  (define handoff-privkey
    (key-pair->private-key handoff-key-pair))

  (define handoff-pubkey
    (key-pair->public-key handoff-key-pair))

  (define our-side-name
    (sha256d (syrup-encode handoff-pubkey)))

  ;; This has added some indirection and promises which are bit slower
  ;; than just handing around the raw value. We may want to consider
  ;; memorizing it.
  (define our-location-sig-vow
    (on our-location-vow
        (lambda (our-location)
          (let ((encoded-location
                 (syrup-encode
                  (make-tagged* 'my-location our-location)
                  #:marshallers marshallers)))
            (sign encoded-location handoff-privkey)))
        #:promise? #t))

  (define core-beh
    (methods
     [(get-suite) 'prot0]
     [(get-our-side-name) our-side-name]
     [(get-handoff-pubkey) handoff-pubkey]
     ;; TODO: Horrible, we need to protect against this
     [(get-handoff-privkey) handoff-privkey]
     [(get-location-sig) our-location-sig-vow]))

  (define pre-init-beh
    (extend-methods core-beh
      [(install-remote-key remote-encoded-key
                           remote-handoff-key
                           remote-location)
       (bcom (ready-beh remote-encoded-key
                        remote-handoff-key
                        remote-location)
             'OK)]))

  (define (ready-beh remote-encoded-key
                     remote-key
                     remote-location)
    (define remote-side-name
      (sha256d (syrup-encode remote-encoded-key)))
    (when (equal? remote-side-name our-side-name)
      (error "Both sides can't share the same name / signing key!"))

    ;; Both sides should converge on the same session name if all goes well
    ;; because both sides should have sorted by bytes
    (define session-name
      (sha256d (apply bytes-append
                      (bytes "prot0")
                      (sort (list remote-side-name our-side-name)
                            bytes<?))))

    ;; NOTE: Every session requires that both ends generate brand
    ;; new keypairs.
    ;; Thus we could probably have a unique derived key per session
    ;; directly from the secret derivation with no additional step?
    ;; But I'm unsure about this.  It may be good hygiene if we
    ;; use a shared keypair to derive a CEK (Content Encryption Key)
    ;; anyway...
    #;(define shared-secret ...)

    (define (make-handoff-base-cert exported-remote-refr)
      ;; TODO: Bail out early if we've already disconnected
      (define exported-captp-connector
        (remote-refr-captp-connector exported-remote-refr))
      (define exported-connector-obj
        (exported-captp-connector 'connector-obj))
      (define receiver-key remote-encoded-key)
      (define exporter-location
        ($$ intra-node-incanter
            exported-connector-obj 'get-remote-location))
      (define gifter-and-exporter-session
        ($$ intra-node-incanter exported-connector-obj
            'get-session-name))
      (define gifter-side
        ($$ intra-node-incanter exported-connector-obj
            'get-our-side-name))
      (define gift-id (strong-random-bytes 32))

      (define handoff-give
        (desc:handoff-give receiver-key
                           exporter-location gifter-and-exporter-session
                           gifter-side
                           gift-id))
      (define handoff-give-sig
        (sign (syrup-encode handoff-give
                            #:marshallers marshallers)
              ($$ intra-node-incanter
                  exported-connector-obj 'get-handoff-privkey)))

      (define exporter-session-bootstrap
        ($$ intra-node-incanter
            exported-connector-obj 'get-remote-bootstrap))

      (unless (exported-captp-connector 'same-connection? exported-remote-refr)
        (error "Tried to deposit a gift not at the remote location"))

      ;; Now we send a message to the exporter saying we'd like to deposit
      ;; this gift
      (<-np exporter-session-bootstrap 'deposit-gift
            gift-id exported-remote-refr)

      (desc:sig-envelope handoff-give handoff-give-sig))

    (define (start-retrieve-handoff signed-handoff-give)
      (assert-type signed-handoff-give signed-handoff-give?)
      (let ((exporter-location
             (desc:handoff-give-exporter-location
              (desc:sig-envelope-signed signed-handoff-give))))
        (on (<- router 'self-location? exporter-location)
            (lambda (self-location?)
              (when self-location?
                ;; Oh, this is us.  Well, we don't need to open a new session
                ;; for that, though we do need to coordinate with whatever
                ;; session is in question
                ;; In order for this to happen, we have to be getting a
                ;; handoff with ourselves as the gifter!  Yikes!  Well,
                ;; this can happen accidentally if A and B have two simultaneous
                ;; sessions open with each other.
                ;;
                ;; Note that this might be caused by the crossed hellos problem
                ;; (or simply that even from the outgoing connection side, we
                ;; don't bother to deduplicate while attempting a connection...
                ;; oops)
                (error "Handoff points at ourselves... crossed hellos or adjacent problem?"))

              ;; Oh, this is someone else.
              ;; Well, we're going to need to make a receive certificate
              ;; and work with the router to pass it along
              (on (<- router 'make-handoff-receive-withdrawal exporter-location)
                  (match-lambda
                    [(handoff-withdrawl sessionmeta)
                     (define receiver-exporter-session-coordinator
                       (match sessionmeta
                         [($ <sessionmeta> _ _ _ coordinator _) coordinator]))
                     (define receiver-exporter-session-name
                       ($$ receiver-exporter-session-coordinator 'get-session-name))
                     (define receiver-exporter-our-side-name
                       ($$ receiver-exporter-session-coordinator 'get-our-side-name))

                     (let* ((handoff-receive
                             (desc:handoff-receive receiver-exporter-session-name
                                                   receiver-exporter-our-side-name
                                                   ($$ our-handoff-count)
                                                   signed-handoff-give))
                            (handoff-receive-sig
                             (sign (syrup-encode handoff-receive
                                                 #:marshallers marshallers)
                                   handoff-privkey))
                            (signed-handoff-receive
                             (desc:sig-envelope handoff-receive
                                                handoff-receive-sig)))
                       ($$ our-handoff-count (1+ ($$ our-handoff-count)))
                       ($$ handoff-withdrawl signed-handoff-receive))])
                    #:promise? #t))
              #:promise? #t)))

    (define (give-handoff-legit? signed-handoff-give)
      (assert-type signed-handoff-give signed-handoff-give?)
      (match-let* ((($ <desc:sig-envelope>
                       (? desc:handoff-give? handoff-give)
                       (? signature-sexp? give-sig-sexp))
                    signed-handoff-give)
                   (($ <desc:handoff-give>
                       _give-recipient-encoded-key
                       give-exporter-location
                       give-session
                       give-gifter-side
                       _give-gift-id)
                    handoff-give)
                   (encoded-handoff-give
                    (syrup-encode handoff-give
                                  #:marshallers marshallers))
                   (give-sig
                    (captp-signature->crypto-signature
                     give-sig-sexp)))
        (on (<- router 'self-location? give-exporter-location)
            (lambda (self-location?)
              (and (equal? session-name give-session)
                   (equal? give-gifter-side remote-side-name)
                   ;; I'm not sure if this one is critical.
                   ;; Should consider the attack scenarios again.
                   ;; Probably doesn't hurt; maybe can just leave it until
                   ;; we find a reason not to.
                   self-location?
                   (verify give-sig encoded-handoff-give remote-key)))
            #:promise? #t)))

    (define (full-handoff-legit? signed-handoff-receive
                                 gifter-exporter-session-coordinator)
      (assert-type signed-handoff-receive signed-handoff-receive?)
      (match-let* ((($ <desc:sig-envelope> (and handoff-receive
                                            ($ <desc:handoff-receive>
                                             (? bytevector? receiver-exporter-session)
                                             (? bytevector? receiver-side-name)
                                             (? integer? this-handoff-count)
                                             signed-handoff-give))
                                       (? signature-sexp? receive-sig-sexp))
                    signed-handoff-receive)
                   (encoded-handoff-receive
                    (syrup-encode handoff-receive
                                  #:marshallers marshallers))
                   (give-receiver-encoded-key
                    (desc:handoff-give-receiver-key
                     (desc:sig-envelope-signed signed-handoff-give)))
                   (give-receiver-key
                    (captp-public-key->crypto-public-key
                     give-receiver-encoded-key))
                   (receive-sig
                    (captp-signature->crypto-signature
                     receive-sig-sexp)))
        (define valid-handoff?
          (on ($$ gifter-exporter-session-coordinator 'give-handoff-legit? signed-handoff-give)
              (lambda (handoff-give-legit?)
                (and handoff-give-legit?
                     ;; Check the desc:handoff-receive was received in the stated session.
                     (equal? session-name receiver-exporter-session)
                     ;; Check the side name is the other side (i.e. not us)
                     (equal? remote-side-name receiver-side-name)
                     ;; Protect replay attacks by verifying remote handoff-count
                     (>= this-handoff-count ($$ gifter-exporter-session-coordinator 'get-remote-handoff-count))
                     ;; Finally verify the signature is valid.
                     (verify receive-sig encoded-handoff-receive give-receiver-key)))
              #:promise? #t))

        ;; If it is in fact a valid handoff, let's increment the count so
        ;; it can't be replayed.
        (on valid-handoff?
            (lambda (valid?)
              (when valid?
                ($$ gifter-exporter-session-coordinator 'new-handoff-count (+ this-handoff-count 1)))))

        valid-handoff?))

    (extend-methods core-beh
      [(get-remote-side-name) remote-side-name]
      [(get-remote-location) remote-location]
      [(get-session-name) session-name]
      [(get-our-side-name) our-side-name]
      [(get-remote-handoff-count) ($$ remote-handoff-count)]
      [(new-handoff-count new-count) ($$ remote-handoff-count new-count)]
      ;; handoff stuff
      [make-handoff-base-cert make-handoff-base-cert]
      [start-retrieve-handoff start-retrieve-handoff]
      [full-handoff-legit? full-handoff-legit?]
      [give-handoff-legit? give-handoff-legit?]))

  pre-init-beh)

(define (^connection-establisher bcom mycapn-vow netlayer netlayer-name)
  (lambda (io remote-connect-location)
    (on mycapn-vow
        (lambda (mycapn)
          (<- ($$ mycapn) 'new-connection netlayer netlayer-name
              io remote-connect-location))
        #:promise? #t)))

(define-actor (^mycapn bcom self netlayer-map registry locator)
  ;; Warden and incanter for collaborating parties in this
  ;; particular node
  (define-values (intra-node-warden intra-node-incanter)
    (spawn-warding-pair))
  (define locations->session-name-resolvers
    (spawn ^ghash))
  (define locations->open-session-names
    (spawn ^ghash))
  (define open-session-names->sessionmeta
    (spawn ^ghash))

  ;; For keeping track of new outbound sessions to detect crossed hellos
  (define locations->crossed-hellos-mitigator
    (spawn ^ghash))

  (define (^bootstrap bcom coordinator)
    (define session-name ($$ coordinator 'get-session-name))
    (define gifts
      (spawn ^ghash))
    (define waiting-gifts
      (spawn ^ghash))

    (define (valid-gift-id? x)
      (or (integer? x)
          (bytevector? x)
          (string? x)))
    ;; In the case of depositing, we're putting a gift at gift-id and an
    ;;
    (define (deposit-gift gift-id obj
                          ;; #:manual-drop? [manual-drop? #f]
                          )
      (assert-type gift-id valid-gift-id?)
      (assert-type obj local-refr?)
      (when ($$ waiting-gifts 'has-key? gift-id)
        (match ($$ waiting-gifts 'ref gift-id)
          [(_gift-promise gift-resolver)
           ($$ gift-resolver 'fulfill obj)
           ($$ waiting-gifts 'remove gift-id)]))
      ($$ gifts 'set gift-id (make-giftmeta obj #t)))

    (define (withdraw-gift signed-handoff-receive)
      (assert-type signed-handoff-receive signed-handoff-receive?)
      (match-let* ((handoff-receive
                    (desc:sig-envelope-signed signed-handoff-receive))
                   (handoff-give
                    (desc:sig-envelope-signed
                     (desc:handoff-receive-signed-give handoff-receive)))
                   (gifter-exporter-session-id
                    (desc:handoff-give-gifter-exporter-session handoff-give))
                   (($ <sessionmeta> cert-session-location
                       cert-session-local-bootstrap-obj
                       cert-session-remote-bootstrap-obj
                       cert-session-coordinator
                       cert-session-session-name)
                    (if ($$ open-session-names->sessionmeta 'has-key? gifter-exporter-session-id)
                        ($$ open-session-names->sessionmeta 'ref gifter-exporter-session-id)
                        (begin
                          (error 'no-open-session "No open session with key ~s"
                                 gifter-exporter-session-id)))))
        ;; TODO: count stuff here too, but needs to be in this session
        (on (<- coordinator 'full-handoff-legit? signed-handoff-receive cert-session-coordinator)
            (lambda (handoff-legit?)
              ;; If we made it this far, it's ok... so time to get
              ;; that referenced object!
              (if handoff-legit?
                  ($$ intra-node-incanter cert-session-local-bootstrap-obj
                      'pull-out-gift
                      (desc:handoff-give-gift-id handoff-give))
                  (error 'invalid-handoff-cert
                         "Handoff cert invalid for session: ~s"
                         signed-handoff-receive)))
            #:promise? #t)))

    (define main-beh
      (methods
       [deposit-gift deposit-gift]
       [withdraw-gift withdraw-gift]
       [(fetch swiss-num)
        ($$ locator 'fetch swiss-num)]))

    (define cross-gift-beh
      (methods
       [(pull-out-gift id)
        (cond
         [($$ gifts 'has-key? id)
          (match-let ((($ <giftmeta> gift destroy-on-fetch?)
                       ($$ gifts 'ref id)))
            (when destroy-on-fetch?
              ($$ gifts 'remove id))
            gift)]
         ;; queue it
         [else
          (if ($$ waiting-gifts 'has-key? id)
              (match ($$ waiting-gifts 'ref id)
                [(gift-promise _gift-resolver)
                 gift-promise])
              (let-values ([(gift-promise gift-resolver)
                            (spawn-promise-and-resolver)])
                ($$ waiting-gifts 'set id (list gift-promise gift-resolver))
                gift-promise))])]))

    (ward intra-node-warden cross-gift-beh #:extends main-beh))

  ;; TODO: Rename this to connect-to-node I guess?
  (define (retrieve-or-setup-session-vow remote-node-loc)
    (if ($$ locations->open-session-names 'has-key? remote-node-loc)
        ;; found an open session for this location
        (let ([session-name-vow ($$ locations->open-session-names
                                    'ref remote-node-loc)])
          (on session-name-vow
              (lambda (session-name)
                (sessionmeta-remote-bootstrap-obj
                 ($$ open-session-names->sessionmeta 'ref session-name)))
              #:promise? #t))
        ;; Guess we'll make a new one
        (let-values ([(netlayer) (get-netlayer-for-location remote-node-loc)]
                     [(session-name-vow session-name-resolver) (spawn-promise-and-resolver)]
                     [(bootstrap-vow bootstrap-resolver) (spawn-promise-and-resolver)])
          ;; To ensure future calls don't create more than one connection
          ;; setup a vow for the session name which will be fulfilled later.
          ;; Once the vow we're creating here is fulfilled we'll swap it out
          ;; for the real value so GC can happen & for minor speed improvements.
          ($$ locations->session-name-resolvers 'set remote-node-loc session-name-resolver)
          ($$ locations->open-session-names 'set remote-node-loc session-name-vow)
          ;; Connect to the node
          (on (<- netlayer 'connect-to remote-node-loc)
              (lambda (session)
                ($$ bootstrap-resolver 'fulfill session))
              #:catch
              (lambda (err)
                ($$ session-name-resolver 'break err)
                ($$ locations->session-name-resolvers 'remove remote-node-loc)
                ($$ locations->open-session-names 'remove remote-node-loc)
                ($$ bootstrap-resolver 'break err)))
          bootstrap-vow)))

  (define (get-netlayer-for-location loc)
    (define transport-tag (ocapn-node-transport loc))
    (unless ($$ netlayer-map 'has-key? transport-tag)
      (error 'unsupported-transport
             "NETLAYER not supported for this node: ~a" transport-tag))
    ($$ netlayer-map 'ref transport-tag))

  (define (self-location? loc)
    (define netlayer (get-netlayer-for-location loc))
    (<- netlayer 'self-location? loc))

  ;; Sturdyref stuff, to be refactored
  ;; TODO: expiry/revocation/unregistry
  (define (register obj netlayer-name)
    (assert-type obj live-refr?)
    (assert-type netlayer-name symbol?)
    (unless ($$ netlayer-map 'has-key? netlayer-name)
      (error 'unsupported-transport
             "NETLAYER not supported for this node: ~a" netlayer-name))
    (let* ((netlayer ($$ netlayer-map 'ref netlayer-name))
           (node-loc (<- netlayer 'our-location))
           (nonce ($$ registry 'register obj)))
      (if (promise-refr? node-loc)
          (on node-loc
              (lambda (node-loc)
                (make-ocapn-sturdyref node-loc nonce))
              #:promise? #t)
          (make-ocapn-sturdyref node-loc nonce))))
  (define (enliven sturdyref-vow)
    (on sturdyref-vow
        (lambda (sturdyref)
          (let ((sref-loc (ocapn-sturdyref-node sturdyref))
                (sref-swiss-num (ocapn-sturdyref-swiss-num sturdyref)))
            ;; Is it local?
            (on (self-location? sref-loc)
                (lambda (self?)
                  (if self?
                      (<- locator 'fetch sref-swiss-num)
                      (<- (retrieve-or-setup-session-vow sref-loc) 'fetch
                          sref-swiss-num)))
                #:promise? #t)))
          #:promise? #t))

  ;; Setup all the netlayers with a connection establisher.
  ;; This needs to be a `<-` because we may have a vow to the ^ghash
  ;; if we're being rehydrated with aurie.
  (on (<- netlayer-map 'data)
      (lambda (netlayer-map-data)
        (ghash-for-each
         (lambda (netlayer-name netlayer)
           (<-np netlayer 'setup (spawn ^connection-establisher self netlayer netlayer-name)))
         netlayer-map-data)))

  (methods
   [(make-handoff-receive-withdrawal remote-location)
    (define (^handoff-receive-withdrawl bcom remote-bootstrap-obj)
      (define (active-beh signed-handoff-receive)
        ;; Feels maybe a little silly to mistrust the use of
        ;; 'make-handoff-receive-withdrawl, but we can so lets mistrust.
        (define handoff-give
          (desc:sig-envelope-signed
           (desc:handoff-receive-signed-give
            (desc:sig-envelope-signed
             signed-handoff-receive))))
        (define exporter-location
          (desc:handoff-give-exporter-location handoff-give))
        (if (equal? exporter-location remote-location)
            (bcom defunct-beh
                  (<- remote-bootstrap-obj 'withdraw-gift signed-handoff-receive))
            (error "Exporter location specified in handoff-receive does not match expected location.")))
      (define (defunct-beh)
        (error "Already used."))
      active-beh)

    ;; "Returning home" should be handled in start-retrieve-handoff
    (define session-bootstrap-vow
      (on (self-location? remote-location)
          (lambda (self?)
            (if self?
                (error "send-handoff-receive called with self-location")
                (retrieve-or-setup-session-vow remote-location)))
          #:promise? #t))
    (on session-bootstrap-vow
        (lambda (remote-bootstrap-obj)
          (define session-name
            ($$ locations->open-session-names 'ref remote-location))
          (define sessionmeta
            ($$ open-session-names->sessionmeta 'ref session-name))
          (list (spawn ^handoff-receive-withdrawl remote-bootstrap-obj)
                sessionmeta))
        #:promise? #t)]

   ;; TODO: we should also allow some way to shut things down here or
   ;; somewhere...
   ;; TODO: Should this still be an exposed method?  Maybe it's something only
   ;; the ^connection-establisher should call...
   [(new-connection netlayer netlayer-name message-io remote-connect-location)
    (define (send-to-remote msg)
      (<-np message-io 'write-message msg marshallers)
      *unspecified*)
    (define our-location-vow
      (<- netlayer 'our-location))
    (define coordinator
      (spawn ^coordinator ($$ self) our-location-vow
             intra-node-warden intra-node-incanter))
    (define handoff-pubkey
      ($$ coordinator 'get-handoff-pubkey))
    (define our-location-sig-vow
      (<- coordinator 'get-location-sig))

    (define-values (remote-bootstrap-vow remote-bootstrap-resolver)
      (spawn-promise-and-resolver))

    ;; Complete the initialization step against the remote node.
    ;; Basically this allows the coordinator to know of what remote
    ;; key will be used in this session.
    (define (^setup-completer bcom)
      (match-lambda
        ;; TODO: Shouldn't the netlayer actually interpret this message
        ;;   before it gets here?  Ie, at this stage, we're already
        ;;   "confident" this is from the right location
        [($ <op:start-session>
            (? string? remote-captp-version)
            remote-encoded-pubkey
            ;; TODO: We want to restores something like the below, which
            ;;   is what the racket version expects, or at least unify the
            ;;   two.
            #;(and remote-encoded-pubkey
            ('eddsa 'public 'ed25519 _))
            (? ocapn-node? claimed-remote-location)
            encoded-remote-location-sig)

         ;; Check we are speaking the same language!
         (unless (string=? remote-captp-version captp-version)
           ;; Needs to be <-np-extern so that the error that is
           ;; thrown after doesn't cancel dispatch.
           (<-np-extern incoming-forwarder
                        (internal-shutdown 'abort "CapTP version is incompatible"))
           (error (format #f "CapTP version is incompatible (our version: ~a, remote version: ~a)"
                          captp-version
                          remote-captp-version)))

         (define remote-handoff-pubkey
           (captp-public-key->crypto-public-key remote-encoded-pubkey))
         ;; TODO: I guess we didn't know by the time this was opened
         ;;   what the remote location was going to be... that's part of the reason
         ;;   for the start-session message...
         ;;   So, remove this if we can.  Or realistically, move this whole part
         ;;   to the NETLAYER code.
         #;(unless (same-node-location? claimed-remote-location remote-location)
         (error (format "Supplied location mismatch. Claimed: ~s Expected: ~s" ; ; ; ;
         claimed-remote-location remote-location)))

         (define encoded-location
           (syrup-encode
            (make-tagged* 'my-location claimed-remote-location)
            #:marshallers marshallers))
         (define remote-location-sig
           (captp-signature->crypto-signature encoded-remote-location-sig))

         (unless (verify remote-location-sig encoded-location remote-handoff-pubkey)
           (let ((reason "Invalid location signature"))
             ;; Needs to be <-np-extern so that the error that is
             ;; thrown after doesn't cancel dispatch.
             (<-np-extern incoming-forwarder
                          (internal-shutdown 'abort reason))
             (error 'captp-invalid-signature remote-location-sig)))

         ;; TODO: Now we need to do the dial back and verify that
         ;; the location is where it says it is!

         ;; Now that we've verified:

         (define remote-location claimed-remote-location)

         ;; Now use it to finish initializing the coordinator
         ($$ coordinator 'install-remote-key
             remote-encoded-pubkey
             remote-handoff-pubkey
             remote-location)

         ;; Handle potential crossed hellos mitigation - incoming connections decide if we
         ;; should continue or abort.
         ;; See the comment below for more information about this (above ^crossed-hellos-mitigator).
         (define can-continue?
           (let* ((chm ($$ locations->crossed-hellos-mitigator 'ref remote-location #f))
                  (their-side-name ($$ coordinator 'get-remote-side-name))
                  (outgoing? (ocapn-node? remote-connect-location))
                  (must-abort? (and chm (not outgoing?) ($$ chm their-side-name))))
             ;; Send internal shutdown if needed.
             (when must-abort?
               ($$ locations->crossed-hellos-mitigator 'remove remote-location)
               (<-np incoming-forwarder (internal-shutdown 'abort "Crossed hellos mitigation")))
             (not must-abort?)))

         (define (make-local-bootstrap-obj)
           (spawn ^bootstrap coordinator))

         (when can-continue?
           (let*-values (((session-name) ($$ coordinator 'get-session-name))
                         ((local-bootstrap-obj) (make-local-bootstrap-obj))
                         ((captp-incoming-handler remote-bootstrap-obj)
                          (setup-captp-conn send-to-remote coordinator
                                            local-bootstrap-obj
                                            intra-node-warden intra-node-incanter)))
             ($$ remote-bootstrap-resolver 'fulfill remote-bootstrap-obj)

             ;; And set things up so that the incoming-forwarder now goes
             ;; to the captp-incoming-handler
             ($$ incoming-swap captp-incoming-handler)

             ;; And now install in the open sessions in the directory
             (let ((resolver ($$ locations->session-name-resolvers 'ref remote-location)))
               (when resolver
                 ($$ resolver 'fulfill session-name)))
             ($$ locations->open-session-names 'set remote-location session-name)
             ($$ open-session-names->sessionmeta 'set
                 session-name
                 (make-sessionmeta remote-location
                                   local-bootstrap-obj remote-bootstrap-obj
                                   coordinator session-name))
             ;; When the connection has severed remove it from the hash of
             ;; sessions, so that in the future we could setup a new connection again.
             (on-sever remote-bootstrap-obj
                 (lambda _
                   (when ($$ locations->crossed-hellos-mitigator 'ref remote-location #f)
                     ($$ locations->crossed-hellos-mitigator 'remove remote-location))
                   ($$ locations->open-session-names 'remove remote-location)
                   ($$ open-session-names->sessionmeta 'remove session-name)))))
         *unspecified*]
        [($ <op:abort> reason)
         (bcom (lambda _ *unspecified*))]
        ;; Handle shutdown requests that happen before the setup
        ;; completer hands control to the internal handler.
        [($ <internal-shutdown> (? symbol? type) (? string? reason))
         (when (eq? type 'abort)
           (send-to-remote (op:abort reason)))
         ;; Since we're shutting down, our new behavior will be to
         ;; ignore all further messages.
         (bcom (lambda _ *unspecified*))]))

    (define-values (incoming-forwarder incoming-swap)
      (swappable (spawn ^setup-completer)))

    (define (read-next-message)
      (on (<- message-io 'read-message unmarshallers)
          (match-lambda
            [(? eof-object?)
             (<-np-extern incoming-forwarder
                          (internal-shutdown 'disconnect "Remote disconnected"))]
            [msg
             (<-np-extern incoming-forwarder msg)
             (read-next-message)])))
    ;; Kick off the reading message loop
    (read-next-message)

    ;; The crossed hellos problem is where we try to connect to a location
    ;; at the same time, they are trying to connect to us. Only one of these
    ;; connections should be allowed to succeed.
    ;;
    ;; In CapTP when each side opens a connection it MUST send its op:start-session
    ;; message first. When each side has received this, it then should look and
    ;; detect crossed hellos (we look in the locations->crossed-hellos-mitigator map).
    ;;
    ;; If we detect the crossed hellos problem we take our key from our outbound
    ;; connection and compute the side name (our-side-name) and the remote key
    ;; from the inbound connection and calculate their name (their-side-name).
    ;; With both of these, we sort them bytewise and whichever is lower, that
    ;; session ends, the higher of the two continues.
    (define (^crossed-hellos-mitigator bcom our-side-name)
      (define voided-beh (lambda _ *unspecified*))
      (lambda (remote-side-name)
        (let ([sorted-names (sort (list our-side-name remote-side-name) bytes<?)])
          (if (equal? (car sorted-names) our-side-name)
              (begin
                (<-np incoming-forwarder (internal-shutdown 'abort "Crossed hellos mitigation"))
                (bcom voided-beh #f))
              (bcom voided-beh #t)))))

    (when (ocapn-node? remote-connect-location)
      ($$ locations->crossed-hellos-mitigator 'set
          remote-connect-location
          (spawn ^crossed-hellos-mitigator ($$ coordinator 'get-our-side-name))))

    ;; Send our op:start-session message to the other side, which will be
    ;; handled by the ^setup-completer above.
    (on (all-of our-location-vow our-location-sig-vow)
        (match-lambda
          [(our-location our-location-sig)
           (send-to-remote (op:start-session captp-version
                                             handoff-pubkey
                                             our-location
                                             our-location-sig))]))
    remote-bootstrap-vow]

   [self-location? self-location?]
   ;; ... is that it?
   [connect-to-node retrieve-or-setup-session-vow]

   [(install-netlayer netlayer)
    (on (<- netlayer 'netlayer-name)
        (lambda (netlayer-name)
          (when ($$ netlayer-map 'has-key? netlayer-name)
            (error (format #f "Already has netlayer key ~a" netlayer-name)))

          ($$ netlayer-map 'set netlayer-name netlayer)
          (<- netlayer 'setup (spawn ^connection-establisher self netlayer netlayer-name)))
        #:promise? #t)]
   [register register]
   [enliven enliven]
   ;; Get the nonce registry used for sturdyrefs to be able to tweak
   ;; it directly.
   [(get-registry) registry]))

(define* (spawn-mycapn #:rest netlayers)
  (define netlayer-map
    (spawn ^ghash
           (fold
            (lambda (netlayer netmap)
              (ghash-set netmap ($$ netlayer 'netlayer-name)
                         netlayer))
            (make-ghash)
            netlayers)))

  ;; For sturdyrefs
  ;; TODO: Eventually... well this whole sturdyref nonsense we want
  ;; to make more configureable
  (define-values (registry locator)
    (spawn-nonce-registry-and-locator))

  ;; Seflish spawn
  (let* ((self (spawn ^cell))
         (mycapn (spawn ^mycapn self netlayer-map registry locator)))
    ($$ self mycapn)
    mycapn))

(define captp-env
  (make-persistence-env
   `((((goblins ocapn captp) ^mycapn) ,^mycapn))
   #:extends (list cell-env common-env nonce-registry-env)))
