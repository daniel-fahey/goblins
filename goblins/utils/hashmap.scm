;;; Copyright 2025 David Thompson <dave@spritely.institute>
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

(define-module (goblins utils hashmap)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-hashmap
            make-hashqmap
            make-hashvmap
            hashmap
            hashqmap
            hashvmap
            hashmap?
            hashmap-ref
            hashmap-set
            hashmap-remove
            hashmap-fold
            hashmap-for-each))

;; Based on Phil Bagwell's "Ideal Hash Trees":
;;
;; See: https://lampwww.epfl.ch/papers/idealhashtrees.pdf
;;
;; Some tips and tricks borrowed from Andy Wingo:
;;
;; See: https://wingolog.org/pub/fash.scm

(define-syntax-rule (define-inline name val)
  (define-syntax name (identifier-syntax val)))

;; For efficiency reasons, maximum hash code size and trie branching
;; factor are limited by the size of the fixnum space.  We make heavy
;; use of bitwise operations, so we want to ensure that trie bitmaps
;; and hash codes are fixnums, not heap allocated bignums.  64-bit
;; platforms have a fixnum space 62 bits wide; 32-bit platforms have
;; 30.  Accounting for the sign bit, we are left with 61 and 29 bits,
;; respectively, for positive small integers.
(cond-expand
 (guile
  (define-syntax compile-time-cond
    (lambda (x)
      (syntax-case x (else)
        ((_ (else body ...))
         #'(begin body ...))
        ((_ (exp body ...) clause ...)
         (if (eval (syntax->datum #'exp) (current-module))
             #'(begin body ...)
             #'(compile-time-cond clause ...))))))
  (compile-time-cond
   ((>= (logcount most-positive-fixnum) 32)
    (define-inline %max-hash-bits 61)
    (define-inline %branch-bits 5))
   (else
    (define-inline %max-hash-bits 29)
    (define-inline %branch-bits 4))))
 (hoot
  ;; Hoot doesn't currently have access to 'eval' at expansion time so
  ;; we simply hardcode these values instead.  As of writing,
  ;; WebAssembly is a 32-bit target.
  (define-inline %max-hash-bits 29)
  (define-inline %branch-bits 4)))
(define-inline %branch-size (ash 1 %branch-bits))
(define-inline %branch-mask (1- %branch-size))
(define-inline %init-shift (- %max-hash-bits %branch-bits))

(define-inlinable (bit i) (ash 1 i))
(define-inlinable (bit-set? bitmap i)
  (logtest (ash 1 i) bitmap))

;; Compute the index into a trie based on the hash code 'x' shifted by
;; 'n' bits.
(define-inlinable (trie-index x n)
  (logand (ash x (- n)) %branch-mask))
;; Compute the index of element 'i' in a sparse vector using 'bitmap'.
(define-inlinable (trie-index->vector-index bitmap i)
  (logcount (logand (1- (bit i)) bitmap)))

;; Functional version of vector-set!
(define-inlinable (vector-set v i x)
  (let ((v* (vector-copy v)))
    (vector-set! v* i x)
    v*))
;; Inserts a new element by displacement.
(define-inlinable (vector-insert v i x)
  (let ((n (vector-length v)))
    (if (zero? n)
        (vector x)
        (let ((v* (make-vector (1+ n))))
          (vector-move-left! v 0 i v* 0)
          (vector-set! v* i x)
          (vector-move-left! v i n v* (1+ i))
          v*))))
;; Removes an element and moves subsequent elements to the left to
;; fill the gap.
(define-inlinable (vector-remove v i)
  (let ((n (vector-length v)))
    (if (eq? n 1)
        #()
        (let ((v* (make-vector (1- n))))
          (vector-move-left! v 0 i v* 0)
          (vector-move-left! v (1+ i) n v* i)
          v*))))

;; A persistent, immutable hashmap that supports custom hash
;; procedures and equivalence predicates.  The mapping is stored as a
;; Hash Array Mapped Trie (HAMT).
(define-record-type <hashmap>
  (%make-hashmap root hash equiv)
  hashmap?
  (root hashmap-root)
  (hash hashmap-hash)
  (equiv hashmap-equiv))

;; Trie nodes are sparse vectors.  The bitmap tracks which elements
;; are occupied and the underlying vector is only as large as the
;; number of bits that are set.
(define-record-type <trie>
  (make-trie bitmap vec)
  trie?
  (bitmap trie-bitmap)
  (vec trie-vec))
(define empty-trie (make-trie 0 #()))

;; In the event of a hash collision, we'll use a chain of key/value
;; pairs and degrade to O(n) time complexity, just like a classic
;; buckets-and-chains mutable hash table.  Assuming a good hash
;; function, collisions are exceedingly rare on 64-bit platforms but
;; more frequent on 32-bit platforms due to the constrained fixnum
;; space.  However, testing on wasm32 has shown that a hashmap with
;; millions of keys has only about a 1-2% collision rate and the
;; resulting chains are small (2-4 items).
(define-record-type <chain>
  (make-chain items)
  chain?
  (items chain-items))

(define (print-hashmap hashmap port)
  (match hashmap
    (($ <hashmap> root)
     (display "#<hashmap" port)
     (let visit ((x root) (k 0))
       (cond
        ;; Arbitrary upper bound of key/value pairs we will print.
        ((= k 10)
         (display " ..." port)
         #f)
        (else
         (match x
           ((key . val)
            (if (zero? k)
                (display " " port)
                (display ", " port))
            (write key port)
            (display ": " port)
            (write val port)
            (1+ k))
           (($ <trie> _ v)
            (let lp ((i 0) (k k))
              (cond
               ((not k) #f)
               ((< i (vector-length v))
                (lp (1+ i) (visit (vector-ref v i) k)))
               (else k))))
           (($ <chain> items)
            (let lp ((items items) (k k))
              (and k
                   (match items
                     (() k)
                     ((x . rest)
                      (lp rest (visit x k)))))))))))
     (display ">" port))))
(set-record-type-printer! <hashmap> print-hashmap)

(define* (make-hashmap #:optional (hash hash) (equiv equal?))
  (%make-hashmap empty-trie hash equiv))
(define (make-hashqmap)
  (make-hashmap hashq eq?))
(define (make-hashvmap)
  (make-hashmap hashv eqv?))

(define-syntax-rule (define-hashmap-syntax name constructor)
  (define-syntax name
    (syntax-rules ()
      ((_) (constructor))
      ((_ (key val) . rest)
       (hashmap-set (name . rest) key val)))))
(define-hashmap-syntax hashmap  make-hashmap)
(define-hashmap-syntax hashqmap make-hashqmap)
(define-hashmap-syntax hashvmap make-hashvmap)

;; TODO: Uncomment when Hoot supports reader macros.
;; ;; Extend reader with #h, #hq, and #hv syntax.
;; (define (read-hashmap ch port)
;;   (case (peek-char port)
;;     ((#\q)
;;      (read-char port)
;;      #`(hashqmap #,@(read-syntax port)))
;;     ((#\v)
;;      (read-char port)
;;      #`(hashvmap #,@(read-syntax port)))
;;     (else
;;      #`(hashmap #,@(read-syntax port)))))
;; (read-hash-extend #\h read-hashmap)

(define* (hashmap-ref hashmap key #:optional default)
  "Return the value associated with @var{key} in @var{hashmap} or
@var{default} if there is no such value."
  (match hashmap
    (($ <hashmap> root hash equiv)
     (define (key-match? key*)
       (or (eq? key key*) (equiv key key*)))
     (let ((h (hash key most-positive-fixnum)))
       (let visit ((shift %init-shift) (x root))
         (match x
           ((key . val)
            ;; If the key matches, return the value.  Otherwise, fail!
            (if (key-match? key) val default))
           (($ <trie> bitmap v)
            (let ((i (trie-index h shift)))
              ;; If element 'i' is occupied, recursively search the
              ;; node stored there.  Otherwise, fail!
              (if (bit-set? bitmap i)
                  (let ((j (trie-index->vector-index bitmap i)))
                    (visit (- shift %branch-bits) (vector-ref v j)))
                  default)))
           (($ <chain> items)
            ;; Search the chain for a matching key or fail.
            (let lp ((items items))
              (match items
                (() default)
                (((key . val) . rest)
                 (if (key-match? key)
                     val
                     (lp rest))))))))))))

(define (hashmap-set hashmap key val)
  "Extend @var{hashmap} by assocating @var{key} with @var{val} and
return a hashmap."
  (match hashmap
    (($ <hashmap> root hash equiv)
     (let ((h (hash key most-positive-fixnum)))
       (define (key-match? key*)
         (or (eq? key key*) (equiv key key*)))
       (define (visit shift x)
         (match x
           ((and (key* . val*) k/v)
            (if (key-match? key*)
                ;; If the key and the value are the same, then the
                ;; insertion is a no-op.  Otherwise, return the new
                ;; key/value pair.
                (and (not (eq? val val*))
                     (cons key val))
                ;; Partial or full hash collision.
                (let ((h* (hash key* most-positive-fixnum)))
                  (if (eqv? h h*)
                      ;; Full hash collision!
                      (make-chain (list (cons key val) k/v))
                      ;; Create a new subtrie containing the existing
                      ;; key/value pair and insert into that.
                      (let ((subtrie (make-trie (bit (trie-index h* shift))
                                                (vector (cons key* val*)))))
                        (visit shift subtrie))))))
           (($ <trie> bitmap v)
            (let* ((i (trie-index h shift))
                   (j (trie-index->vector-index bitmap i)))
              (if (bit-set? bitmap i)
                  ;; Occupied.
                  (match (visit (- shift %branch-bits) (vector-ref v j))
                    ;; No-op fallthrough
                    (#f #f)
                    (new (make-trie bitmap (vector-set v j new))))
                  ;; Unoccupied.  Insert a new leaf node.
                  (make-trie (logior bitmap (bit i))
                             (vector-insert v j (cons key val))))))
           ((and chain ($ <chain> (and items ((key* . _) . _))))
            ;; Check for hash collision
            (let ((h* (hash key* most-positive-fixnum)))
              (if (eqv? h h*)
                  ;; Hash collision!  Add to the chain or replace the
                  ;; existing value with a new one.
                  (and=> (let lp ((items items))
                           (match items
                             ;; Key not found.  Add a new key/value pair.
                             (() (list (cons key val)))
                             (((and (key* . val*) k/v) . rest)
                              (if (key-match? key*)
                                  ;; No-op if value is the same, too.
                                  (and (not (eq? val val*))
                                       ;; Replace value for key.
                                       (cons (cons key val) rest))
                                  (match (lp rest)
                                    ;; No-op fallthrough.
                                    (#f #f)
                                    (items (cons k/v items)))))))
                         make-chain)
                  ;; Hashes do not collide. Create a new subtrie
                  ;; containing the existing chain and insert into
                  ;; that.
                  (let ((subtrie (make-trie (bit (trie-index h* shift))
                                            (vector chain))))
                    (visit shift subtrie)))))))
       (match (visit %init-shift root)
         ;; Caller attempted to insert something that was already in the
         ;; hashmap, so it's a no-op.
         (#f hashmap)
         (root (%make-hashmap root hash equiv)))))))

(define (hashmap-remove hashmap key)
  "Remove the association for @var{key} from @var{hashmap} and return a
hashmap."
  (match hashmap
    (($ <hashmap> root hash equiv)
     (define (key-match? key*)
       (or (eq? key key*) (equiv key key*)))
     (let ((h (hash key most-positive-fixnum)))
       (define (visit shift x)
         (match x
           ;; Leaf:
           ((key . _)
            (key-match? key))
           ;; Branch:
           (($ <trie> bitmap v)
            (let* ((i (trie-index h shift))
                   (j (trie-index->vector-index bitmap i)))
              (define (remove)
                (make-trie (logand bitmap (lognot (bit i)))
                           (vector-remove v j)))
              (define (update new)
                (make-trie bitmap (vector-set v j new)))
              ;; Check if index i is occupied.  If not, then there is
              ;; nothing to remove.
              (and (bit-set? bitmap i)
                   (match (visit (- shift %branch-bits) (vector-ref v j))
                     ;; No-op fallthrough.
                     (#f #f)
                     ;; Child node has been pruned.
                     (#t
                      (case (logcount bitmap)
                        ;; Maybe prune this node, too, unless it's the
                        ;; root.
                        ((1)
                         (or (not (eq? x root)) empty-trie))
                        ((2)
                         ;; Collapse this subtrie into a leaf
                         ;; node, unless it's the root.
                         (if (eq? x root)
                             (remove)
                             ;; Find the remaining element.
                             (if (eq? j 0)
                                 (vector-ref v 1)
                                 (vector-ref v 0))))
                        (else (remove))))
                     ;; New leaf: (collapsed subtrie)
                     ((? pair? k/v)
                      (case (logcount bitmap)
                        ;; Maybe collapse this node, too, unless it's
                        ;; the root.
                        ((1)
                         (if (eq? x root)
                             (update k/v)
                             k/v))
                        (else (update k/v))))
                     ;; New branch:
                     (branch (update branch))))))
           (($ <chain> items)
            ;; Search the chain to see if the key is present.
            (match (let lp ((items items))
                     (match items
                       ;; Key not found.  This is a no-op.
                       (() #f)
                       (((and (key . _) k/v) . rest)
                        (if (key-match? key)
                            ;; Key matches.  Drop the pair.
                            rest
                            ;; Keep searching.
                            (match (lp rest)
                              ;; No-op fallthrough.
                              (#f #f)
                              (items (cons k/v items)))))))
              ;; No-op fallthrough.
              (#f #f)
              ;; Collapse chain into a simple leaf node.
              ((k/v) k/v)
              ;; Make a new chain with one less item than before.
              (items (make-chain items))))))
       (match (visit %init-shift root)
         (#f hashmap)
         (root (%make-hashmap root hash equiv)))))))

(define (hashmap-fold proc init hashmap)
  "Apply @var{proc} to the key/value pairs of @var{hashmap} to accumulate
a result and return it.

The accumulator's initial value is @var{init} and each call to
@var{proc} is passed the previous accumulator value.  @var{proc} is
called like so: @code{(proc key value previous)}."
  (match hashmap
    (($ <hashmap> root)
     (let visit ((x root) (prev init))
       (match x
         ((key . val)
          (proc key val prev))
         (($ <trie> _ v)
          (let lp ((i 0) (prev prev))
            (if (< i (vector-length v))
                (lp (1+ i) (visit (vector-ref v i) prev))
                prev)))
         (($ <chain> items)
          (let lp ((items items) (prev prev))
            (match items
              (() prev)
              ((x . rest)
               (lp rest (visit x prev)))))))))))

(define (hashmap-for-each proc hashmap)
  "Apply @var{proc} to the key/value pairs of @var{hashmap} for
side-effects only.  Zero values are returned.

@var{proc} is called like so: @code{(proc key value)}."
  (match hashmap
    (($ <hashmap> root)
     (let visit ((x root))
       (match x
         ((key . val)
          (proc key val)
          (values))
         (($ <trie> _ v)
          (do ((i 0 (1+ i)))
              ((= i (vector-length v)))
            (visit (vector-ref v i))))
         (($ <chain> items)
          (let lp ((items items))
            (match items
              (() (values))
              ((x . rest)
               (visit x)
               (lp rest))))))))))
