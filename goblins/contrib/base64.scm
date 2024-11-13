;;; From Chickadee Game Toolkit (0ec29230d7e5b4706cdedc7a3c380e90e8313d73)
;;; Copyright © 2024 David Thompson <dthompson2@worcester.edu>
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

(define-module (goblins contrib base64)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:export (base64-alphabet
            base64-url-alphabet
            base64-decode
            base64-encode))

(define base64-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(define base64-url-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

(define (alphabet-for-each proc alphabet)
  (if (= (string-length alphabet) 64)
      (let lp ((i 0) (chars (string->list alphabet)))
        (when (< i 64)
          (let* ((char (car chars))
                 (x (char->integer char)))
            ;; FIXME?: Only characters in the
            ;; ASCII range are supported.
            (unless (< x 128)
              (error "invalid base64 alphabet character" char))
            (proc i x)
            (lp (1+ i) (cdr chars)))))
      (error "invalid base64 alphabet" alphabet)))

(define-syntax-rule (define/memoize1 (name arg) body ...)
  (define name
    (let ((cache (make-hash-table)))
      (lambda (arg)
        (or (hash-ref cache arg)
            (let ((val (begin body ...)))
              (hash-set! cache arg val)
              val))))))

(define/memoize1 (alphabet->decoder alphabet)
  (let ((bv (make-bytevector 128 -1)))
    (alphabet-for-each (lambda (i x)
                         (bytevector-s8-set! bv x i))
                       alphabet)
    bv))

(define* (base64-decode str #:key (alphabet base64-alphabet) (padding? #t))
  "Decode the base64 encoded string @var{str} using @var{alphabet} and
return a bytevector containing the decoded data.  If @var{padding?} is
@code{#t} then trailing padding characters are expected in the input."
  (define decoder (alphabet->decoder alphabet))
  (define (fail) (error "invalid base64" str))
  (define (decode char)
    (unless (char? char) (fail))
    (let ((x (bytevector-s8-ref decoder (char->integer char))))
      (when (eq? x -1) (fail))
      x))
  (define pad?
    (if padding?
        (lambda (x) (eq? x #\=))
        eof-object?))
  (call-with-input-string str
    (lambda (in)
      (call-with-output-bytevector
       (lambda (out)
         (let lp ()
           (unless (eof-object? (peek-char in))
             (let* ((a (read-char in))
                    (b (read-char in))
                    (c (read-char in))
                    (d (read-char in)))
               (cond
                ((pad? d)
                 (if (pad? c)
                     ;; 2 bytes of padding.
                     (let ((x (logior (ash (decode a) 6) (decode b))))
                       (put-u8 out (ash (logand x #xff0) -4)))
                     ;; 1 byte of padding.
                     (let ((x (logior (ash (decode a) 12)
                                      (ash (decode b) 6)
                                      (decode c))))
                       (put-u8 out (ash (logand x #x3fc00) -10))
                       (put-u8 out (ash (logand x #x3fc) -2))))
                 ;; Input should be fully consumed at this point.
                 (unless (eof-object? (peek-char in)) (fail)))
                (else
                 (let ((x (logior (ash (decode a) 18)
                                  (ash (decode b) 12)
                                  (ash (decode c) 6)
                                  (decode d))))
                   (put-u8 out (ash (logand x #xff0000) -16))
                   (put-u8 out (ash (logand x #x00ff00) -8))
                   (put-u8 out (logand x #x0000ff))
                   (lp))))))))))))

(define/memoize1 (alphabet->encoder alphabet)
  (let ((bv (make-bytevector 64)))
    (alphabet-for-each (lambda (i x)
                         (bytevector-u8-set! bv i x))
                       alphabet)
    bv))

(define* (base64-encode bv #:key (alphabet base64-alphabet) (padding? #t))
  "Encode the bytevector @var{bv} as base64 using
@var{alphabet} and return a string containing the encoded data.  If
@var{padding?} is @code{#t} then trailing padding characters are
included in the output."
  (define encoder (alphabet->encoder alphabet))
  (define len (bytevector-length bv))
  (define groups (quotient len 3))
  (define padding (- 3 (remainder len 3)))
  (define (encode x) (integer->char (bytevector-u8-ref encoder x)))
  (call-with-output-string
    (lambda (port)
      (let lp ((i 0))
        (let ((offset (* i 3)))
          (cond
           ((< i groups)
            (let ((x (bytevector-uint-ref bv offset (endianness big) 3)))
              (put-char port (encode (ash (logand x #xfc0000) -18)))
              (put-char port (encode (ash (logand x #x03f000) -12)))
              (put-char port (encode (ash (logand x #x000fc0) -6)))
              (put-char port (encode (logand x #x00003f)))
              (lp (1+ i))))
           ((eq? padding 1)
            (let ((x (bytevector-u16-ref bv offset (endianness big))))
              (put-char port (encode (ash (logand x #xfc00) -10)))
              (put-char port (encode (ash (logand x #x03f0) -4)))
              (put-char port (encode (ash (logand x #x000f) 2)))
              (when padding?
                (put-char port #\=))))
           ((eq? padding 2)
            (let ((x (bytevector-u8-ref bv offset)))
              (put-char port (encode (ash (logand x #xfc) -2)))
              (put-char port (encode (ash (logand x #x03) 4)))
              (when padding?
                (put-string port "=="))))))))))
