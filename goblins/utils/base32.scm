;;; Copyright 2023 Christine Lemmer-Webber
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

(define-module (goblins utils base32)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:export (base32-encode
            base32-decode))

(define (make-b32-decode-table b32-alphabet)
  (let ((ht (make-hash-table))
        (alpha-len (string-length b32-alphabet)))
    (do ((i 0 (1+ i)))
        ((= i alpha-len))
      (hashv-set! ht (string-ref b32-alphabet i) i))
    ht))

(define b32-alphabet
  "abcdefghijklmnopqrstuvwxyz234567")
(define b32-alphabet-table
  (make-b32-decode-table b32-alphabet))

(define (b32-alphabet-ref char)
  (hashv-ref b32-alphabet-table char))

(define (in->port in)
  (match in
    ((? string?)
     (open-input-string in))
    ((? bytevector?)
     (open-bytevector-input-port in))
    ((? port?) in)))

;; Helper procedure.  Takes a bytevector (which always has to be
;; less then 8 bytes long, but in our case it always will be) and
;; converts it to a u64 which we use for shifting around
(define (bytevector->u64 bv)
  (define dest-bv (make-bytevector 8))
  (bytevector-copy! bv 0 dest-bv 0 (bytevector-length bv))
  (bytevector-u64-ref dest-bv 0 (endianness big)))

(define chop-mask #b11111)   ; We only want the last 5 bits.

(define* (output-5bv! bv op pad? #:key (alphabet b32-alphabet))
  (define bv-len (bytevector-length bv))
  (define u64 (bytevector->u64 bv))       ; for shifting out 5bit chunks
  (define (u64-5ref shift)                ; get a shifted 5bit chunk
    (logand (ash u64 shift) chop-mask))
  (define (put-5ref shift)                ; put a shifted 5bit chunk
    (put-char op (string-ref b32-alphabet (u64-5ref shift))))
  (match bv-len
    (5 (for-each put-5ref '(-59 -54 -49 -44 -39 -34 -29 -24)))
    (4 (for-each put-5ref '(-59 -54 -49 -44 -39 -34 -29))
       (when pad?
         (display "=" op)))
    (3 (for-each put-5ref '(-59 -54 -49 -44 -39))
       (when pad?
         (display "===" op)))
    (2 (for-each put-5ref '(-59 -54 -49 -44))
       (when pad?
         (display "====" op)))
    (1 (for-each put-5ref '(-59 -54))
       (when pad?
         (display "======" op)))))

(define (_base32-encode in-port out-port pad?)
  (do ((next-5bv (get-bytevector-n in-port 5)
                 (get-bytevector-n in-port 5)))
      ((eof-object? next-5bv))
    (output-5bv! next-5bv out-port pad?)))

(define* (base32-encode in #:key out-port pad?)
  "Base32 encode IN.

IN may be a bytevector, input port, or string.

Writes to OUT-PORT if provided, otherwise returns a string.
Will pad with = symbols if PAD? is #t."
  (define in-port (in->port in))
  (if out-port
      (_base32-encode in-port out-port pad?)
      (call-with-output-string
        (lambda (op)
          (_base32-encode in-port op pad?)))))


(define (_base32-decode in-port out-port)
  ;; Here we're writing 5 bits per character in the base32 string
  ;; re-compacted back onto 8 bit bytes.
  ;; We have to build up a "buffer" since multiple characters are
  ;; "written" on top of each byte before it's ready to write.
  (let lp ((buf #b00000000)    ; the "byte to be" buffer 
           (buf-pos 0))        ; where we're writing to the buffer
    (match (get-char in-port)
      ;; We're done!
      ((? eof-object?)
       'done)
      ;; Skip = and - characters
      ((or #\= #\-) (lp buf buf-pos))
      (char
       ;; n is the current number represented by this character
       (define n
         (or (b32-alphabet-ref char)
             (error "Not a valid base32 character:" char)))
       ;; This determines where the *next* 5 bits will start, but also
       ;; detects if we "finish" this buffer and move onto the next one
       (define-values (write-flag next-buf-pos)
         (euclidean/ (+ buf-pos 5) 8))
       (define write? (= write-flag 1))
       ;; n is only 5 bits long, so say n is #b11111.
       ;; what that really means is that n is #b00011111
       ;; when aligned along the 8 bit byte to "start" with.
       ;; So in addition to the position, we have to "shift back"
       ;; 3 bits.
       (define shift-this-buf (- 3 buf-pos))
       (if write?
           ;; It's time to write!  That means completing the buffered
           ;; byte (and writing it) and starting the new one with the
           ;; overflow, if any
           (begin
             (let (;; The current buffered byte, completed
                   (byte-to-write
                    (logior buf (ash n shift-this-buf)))
                   ;; Shift everyting over for the new buffer
                   ;; cutting off anything bigger than a byte
                   (new-buf (logand (ash n (- 8 next-buf-pos)) #xff)))
               (put-u8 out-port byte-to-write)
               ;; loop with the new buffer
               (lp new-buf next-buf-pos)))
           ;; Guess it's just time to loop again, not yet time
           ;; to write
           (lp (logior buf (ash n shift-this-buf))
               next-buf-pos))))))

(define* (base32-decode in #:key out-port)
  "Base32 decode IN.

IN may be a bytevector, input port, or string.

Writes to OUT-PORT if provided, otherwise returns a string.
Will pad with = symbols if PAD? is #t."
  (define in-port (in->port in))
  (if out-port
      (_base32-decode in-port out-port)
      (call-with-output-bytevector
        (lambda (op)
          (_base32-decode in-port op)))))
