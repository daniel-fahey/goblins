;;; (C) 2020-2022 Christine Lemmer-Webber
;;; Licensed under Apache v2

;; Pulled from:
;;   https://github.com/ocapn/syrup/blob/master/impls/guile/syrup.scm
;; Eventually we should get out a separate package for just guile-syrup
;; but in the meanwhile this simplifies our life a little bit.

(define-module (goblins contrib syrup)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)          ; lists
  #:use-module (srfi srfi-9)          ; records
  #:use-module (srfi srfi-9 gnu)      ; record extensions
  #:use-module (srfi srfi-64)         ; tests
  #:use-module (ice-9 control)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 vlist)
  #:use-module (goblins ghash)
  #:use-module (rnrs bytevectors)

  #:export (;;; The main procedures
            ;;; -------------------
            syrup-encode
            syrup-decode
            syrup-read
            syrup-write

            ;;; Helper datastructure wrappers
            ;;; -----------------------------
            ;;; . o O (Move into their own module?)
            ;; syrec (Syrup Records)
            make-syrec
            make-syrec*
            <syrec>
            syrec?
            syrec-label syrec-args

            ;; pseudosingles (pretend to be a single precision float)
            make-pseudosingle pseudosingle?
            psuedosingle->float

            ;; sets
            make-set set?
            set-add set-remove set-fold set->list set-member?))

;;; Data format
;;; ===========

;; Booleans: t or f
;; Single flonum: F<ieee-single-float> (big endian)
;; Double flonum: D<ieee-double-float> (big endian)
;; Positive integer: <int>+
;; Negative integer: <int>-
;; Bytestrings: 3:cat
;; Strings: 3"cat
;; Symbols: 3'cat
;; Dictionary: {<key1><val1><key1><val1>}
;; Lists: [<item1><item2><item3>]
;; Records: <<label><val1><val2><val3>> (the outer <> for realsies tho)
;; Sets: #<item1><item2><item3>$

(define-syntax-rule (define-char-bv id char)
  (define id
    (make-bytevector 1 (char->integer char))))

(define-char-bv plus-bv #\+)
(define-char-bv minus-bv #\-)
(define-char-bv squarebrac-left-bv #\[)
(define-char-bv squarebrac-right-bv #\])
(define-char-bv curly-left-bv #\{)
(define-char-bv curly-right-bv #\})
(define-char-bv anglebrac-left-bv #\<)
(define-char-bv anglebrac-right-bv #\>)
(define-char-bv doublequote-bv #\")
(define-char-bv singlequote-bv #\')
(define-char-bv colon-bv #\:)
(define-char-bv F-bv #\F)
(define-char-bv D-bv #\D)
(define-char-bv f-bv #\f)
(define-char-bv t-bv #\t)
(define-char-bv hash-bv #\#)
(define-char-bv dollar-bv #\$)

#;(define zero-plus-bv
  )

;;; Syrup records
;;; =============

;; Representation of syrup records... coersion to and from other
;; datastructures is left as an exercise to the reader (for now at least)
(define-record-type <syrec>
  (make-syrec label args)
  syrec?
  (label  syrec-label)
  (args syrec-args))

(define (make-syrec* tag . args)
  (make-syrec tag args))


;;; TODO: Move all these

;;; Other datastructures
;;; ====================

;; pseudosingle is just to express that, from a syrup perspective,
;; this is encoded as single precision floating point... even though
;; it's really double precision floating point.
(define-record-type <pseudosingle>
  (_make-pseudosingle float)
  pseudosingle?
  (float pseudosingle-float))

(define (make-pseudosingle float)
  (unless (inexact? float)
    (error "Not a valid number to wrap in a pseudosingle" float))
  (_make-pseudosingle float))

(define (pseudosingle->float psing)
  (pseudosingle-float psing))

;;; An extremely meh implementation of sets

;;; TODO: make and replace with "gsets"

(define-record-type <set>
  (_make-set ht)
  set?
  (ht _set-ht))

(define (print-set set port)
  (define items
    (vhash-fold
     (lambda (k _v prev)
       (cons k prev))
     '()
     (_set-ht set)))
  (format port "#<set ~a>" items))

(set-record-type-printer! <set> print-set)


(define (make-set . items)
  (define vh
    (fold
     (lambda (item vh)
       (vhash-consq item #t vh))
     vlist-null items))
  (_make-set vh))

(define (set-add set item)
  (_make-set (vhash-cons item #t (_set-ht set))))

(define (set-remove set item)
  (_make-set (vhash-delete (_set-ht set) item)))

(define (set-fold proc init set)
  (vhash-fold
   (lambda (key _val prev)
     (proc key prev))
   init
   (_set-ht set)))

(define (set->list set)
  (vhash-fold
   (lambda (key _val prev)
     (cons key prev))
   '()
   (_set-ht set)))

(define (set-member? set key)
  (match (vhash-assoc key (_set-ht set))
    [(_val . #t)
     #t]
    [#f #f]))


;;; bytevector utils
;;; ================

(define (bytes-append . bvs)
  (define new-bv-len
    (fold
     (lambda (x prev)
       (+ (bytevector-length x) prev))
     0 bvs))
  (define new-bv
    (make-bytevector new-bv-len))
  (let lp ([cur-pos 0]
           [bvs bvs])
    (match bvs
      [(this-bv . next-bvs)
       (define this-bv-len
         (bytevector-length this-bv))
       (bytevector-copy! this-bv 0 new-bv cur-pos this-bv-len)
       ;; move onto next bytevector
       (lp (+ cur-pos this-bv-len)
           next-bvs)]
      ['() 'done]))
  new-bv)

(define* (netstring-encode bstr #:key [joiner colon-bv])
  (define bstr-len
    (bytevector-length bstr))
  (define bstr-len-as-bytes
    (string->bytes/latin-1 (number->string bstr-len)))
  (bytes-append bstr-len-as-bytes
                joiner
                bstr))

(define (string->bytes/latin-1 str)
  (string->bytevector str "ISO-8859-1"))
(define (string->bytes/utf-8 str)
  (string->bytevector str "UTF-8"))
(define (bytes->string/utf-8 bstr)
  (bytevector->string bstr "UTF-8"))

;; alias for simplicity
(define bytes string->bytes/latin-1)

;; Test: 
#;(bytevector->string
 (netstring-encode
  (string->bytevector "Hello world!" "ISO-8859-1"))
 "ISO-8859-1")
;; => "12:Hello world!"

(define (bytes<? bstr1 bstr2)
  (define bstr1-len
    (bytevector-length bstr1))
  (define bstr2-len
    (bytevector-length bstr2))
  (let lp ([pos 0])
    (cond
     ;; we've reached the end of both and they're the same bytestring
     ;; but this isn't <=?
     [(and (eqv? bstr1-len pos)
           (eqv? bstr2-len pos))
      #f]
     ;; we've reached the end of bstr1 but not bstr2, so yes it's less
     [(eqv? bstr1-len pos)
      #t]
     ;; we've reached the end of bstr2 but not bstr1, so no
     [(eqv? bstr2-len pos)
      #f]
     ;; ok, time to compare bytes
     [else
      (let ([bstr1-byte (bytevector-u8-ref bstr1 pos)]
            [bstr2-byte (bytevector-u8-ref bstr2 pos)])
        (if (eqv? bstr1-byte bstr2-byte)
            ;; they're the same, so loop
            (lp (1+ pos))
            ;; otherwise, just compare nubmers
            (< bstr1-byte bstr2-byte)))])))


;;; Encoding
;;; ========

(define* (syrup-encode obj #:key [marshallers '()])
  (define (build-encode-hash hash-ref hash-fold)
    (lambda (obj)
      (let* ([keys-and-encoded
              (hash-fold
               (lambda (key _val prev)
                 (cons (cons (syrup-encode key)
                             key)
                       prev))
               '()
               obj)]
             [sorted-keys-and-encoded
              (sort keys-and-encoded
                    (match-lambda*
                      [((encoded1 . _k1)
                        (encoded2 . _k2))
                       (bytes<? encoded1 encoded2)]))]
             [encoded-hash-pairs
              (fold-right
               (lambda (ke prev)
                 (match ke
                   [(enc-key . key)
                    (let ([val (hash-ref obj key)])
                      (cons (bytes-append enc-key (syrup-encode val))
                            prev))]))
               '()
               sorted-keys-and-encoded)])
        (bytes-append curly-left-bv
                      (apply bytes-append encoded-hash-pairs)
                      curly-right-bv))))
  (define encode-hash
    (build-encode-hash hash-ref hash-fold))
  (define encode-vhash
    (build-encode-hash
     ;; we shouldn't need a not-found case here
     (lambda (vh key)
       (match (vhash-assoc key vh)
         [(_ . val) val]))
     vhash-fold))
  (define encode-ghash
    (build-encode-hash ghash-ref ghash-fold))
  (define (encode obj)
    (match obj
      ;; Bytes are like <bytes-len>:<bytes>
      [(? bytevector?)
       (netstring-encode obj)]
      [0 (string->bytes/latin-1 "0+")]
      ;; Integers are like <integer>+ or <integer>-
      [(? integer?)
       (if (positive? obj)
           (bytes-append (string->bytes/latin-1 (number->string obj)) plus-bv)
           (bytes-append (string->bytes/latin-1 (number->string (* obj -1))) minus-bv))]
      ;; Lists are like [<item1><item2><item3>]
      [(? pair?)
       (bytes-append squarebrac-left-bv
                     (apply bytes-append
                            (map encode obj))
                     squarebrac-right-bv)]
      ;; Dictionaries are like {<key1><val1><key2><val2>}
      ;; We sort by the key being fully encoded.
      [(? hash-table?)
       (encode-hash obj)]
      [(? ghash?)
       (encode-ghash obj)]
      ;; Strings are like <encoded-bytes-len>"<utf8-encoded>
      [(? string?)
       (netstring-encode (string->bytes/utf-8 obj)
                         #:joiner doublequote-bv)]
      ;; Symbols are like <encoded-bytes-len>'<utf8-encoded>
      [(? symbol?)
       (netstring-encode (string->bytes/utf-8
                          (symbol->string obj))
                         #:joiner singlequote-bv)]
      ;; Single flonum floats are like F<big-endian-encoded-single-float>
      [(? pseudosingle?)
       (let ([bv (make-bytevector 4)])
         (bytevector-ieee-single-set! bv 0 obj (endianness big))
         (bytes-append F-bv bv))]
      ;; Double flonum floats are like D<big-endian-encoded-double-float>
      [(and (? number?) (? inexact?))
       (let ([bv (make-bytevector 8)])
         (bytevector-ieee-double-set! bv 0 obj (endianness big))
         (bytes-append D-bv bv))]
      ;; Records are like <<tag><arg1><arg2>> but with the outer <> for realsies
      [(? syrec?)
       (bytes-append anglebrac-left-bv
                     (encode (syrec-label obj))
                     (apply bytes-append
                            (map encode (syrec-args obj)))
                     anglebrac-right-bv)]
      ;; #t is t, #f is f
      [#t t-bv]
      [#f f-bv]
      ;; Sets are like #<item1><item2><item3>$
      [(? set?)
       (let* ([encoded-items
               (set-fold
                (lambda (item prev)
                  (cons (encode item)
                        prev))
                '() obj)]
              [sorted-items
               (sort encoded-items
                     bytes<?)])
         (bytes-append hash-bv
                       (apply bytes-append sorted-items)
                       dollar-bv))]
      [_
       (call/ec
        (lambda (return)
          (for-each (match-lambda
                      ((handles-it? . translate)
                       (when (handles-it? obj)
                         (let ((translated (translate obj)))
                           (if (record? translated)
                               (return (encode translated))
                               (error 'syrup-marshaller-returned-unsupported-type))))))
                    marshallers)
          (error "Unsupported Syrup type:" obj)))]))
  (encode obj))

(define* (syrup-write obj out-port #:key (marshallers '()))
  (put-bytevector out-port
                  (syrup-encode obj #:marshallers marshallers))
  (force-output out-port))


(define-syntax-rule (define-char-matcher proc-name char-set)
  (define (proc-name char)
    (char-set-contains? char-set char)))

(define-char-matcher digit-char? char-set:digit)
;; This path is much more liberal than what we allow in syrup.rkt or syrup.py:
;;   (define-char-matcher whitespace-char? char-set:whitespace)
;; So we're going to be conservative for now...
;; but does it really matter?  I mean who cares.  The whitespace will
;; never appear in the normalized version...
(define-char-matcher whitespace-char?
  (string->char-set " \t\n"))


(define* (syrup-read in-port #:key (unmarshallers '()))
  (call/ec
   (lambda (return-early)
     (define (return-eof)
       (return-early the-eof-object))
     (define (_read-char)
       (match (get-u8 in-port)
         [(? eof-object?) (return-eof)]
         [char-int (integer->char char-int)]))
     (define (_peek-char)
       (match (peek-char in-port)
         [(? eof-object?) (return-eof)]
         [char char]))
     (define read-byte get-u8)
     (define (read-bytes n port)
       (get-bytevector-n port n))
     (define (read-next)
       ;; consume whitespace
       (let lp ()
         (when (whitespace-char? (_peek-char))
           (_read-char)
           (lp)))

       (match (_peek-char)
         ;; it's either a bytestring, a symbol, a string, or an integer...
         ;; we tell via the divider
         [(? digit-char?)
          (let* ((type #f)
                 (int-prefix
                  (string->number
                   (list->string
                    (let lp ()
                      (match (_read-char)
                        ;; Oh, it's a plus... that means it's a positive
                        ;; integer.  Ok.
                        [#\+
                         (set! type 'positive-int)
                         '()]
                        ;; Or the inverse for a minus.
                        [#\-
                         (set! type 'negative-int)
                         '()]
                        [#\:
                         (set! type 'bstr)
                         '()]
                        [#\'
                         (set! type 'sym)
                         '()]
                        [#\"
                         (set! type 'str)
                         '()]
                        [(? digit-char? digit-char)
                         (cons digit-char
                               (lp))]
                        [other-char
                         (error 'syrup-invalid-digit
                                "Invalid digit at pos ~a: ~a"
                                (file-position in-port)
                                other-char)]))))))
            (match type
              ;; it's positive, so just return as-is
              ['positive-int int-prefix]
              ;; it's negative, so invert
              ['negative-int (* int-prefix -1)]
              ;; otherwise it's some byte-length thing
              [_
               (define bstr (read-bytes int-prefix in-port))
               (match type
                 ['bstr
                  bstr]
                 ['sym
                  (string->symbol (bytes->string/utf-8 bstr))]
                 ['str
                  (bytes->string/utf-8 bstr)])]))]
         ;; it's a list
         [(or #\[ #\( #\l)
          (read-byte in-port)
          (let lp ()
            (match (_peek-char)
              ;; We've reached the end
              [(or #\] #\) #\e)
               (read-byte in-port)
               '()]
              ;; one more loop
              [_
               (cons (read-next) (lp))]))]
         ;; it's a hashmap/dictionary
         [(or #\{ #\d)
          (read-byte in-port)
          (let lp ((ht (make-ghash)))
            (match (_peek-char)
              [(or #\} #\e)
               (read-byte in-port)
               ht]
              [_
               (let ((key (read-next))
                     (val (read-next)))
                 (lp (ghash-set ht key val)))]))]
         ;; it's a record
         [#\<
          (read-byte in-port)
          (let ((label (read-next))
                (args (let lp ()
                        (match (_peek-char)
                          [#\>
                           (read-byte in-port)
                           '()]
                          [_ (cons (read-next) (lp))]))))
            (call/ec
             (lambda (return)
               (for-each
                (match-lambda
                  [((and (or (? symbol?) (? string?) (? number?)
                             (? boolean?) (? bytevector?))
                         expected-label)
                    . derecordify)
                   (when (equal? label expected-label)
                     (return (apply derecordify args)))]
                  [(label-pred? . derecordify)
                   (when (label-pred? label)
                     (return (apply derecordify args)))])
                unmarshallers)
               ;; no handler, return as record
               (make-syrec label args))))]
         ;; it's a single float
         [#\F
          (read-byte in-port)
          (bytevector-ieee-double-ref (get-bytevector-n in-port 4) 0
                                      (endianness big))]
         ;; it's a double float
         [#\D
          (read-byte in-port)
          (bytevector-ieee-double-ref (get-bytevector-n in-port 8) 0
                                      (endianness big))]
         ;; it's a boolean
         [#\t
          (read-byte in-port)
          #t]
         [#\f
          (read-byte in-port)
          #f]
         ;; it's a set
         [#\#
          (read-byte in-port)
          (let lp ([s (make-set)])
            (match (_peek-char)
              [#\$
               (read-byte in-port)
               s]
              [_
               (lp (set-add s (read-next)))]))]
         [_
          (error 'syrup-invalid-char "Unexpected character at position ~a: ~a"
                 (file-position in-port)
                 (_peek-char))]))
     (read-next))))

(define* (syrup-decode bstr #:key (unmarshallers '()))
  (define bstr-port
    (open-bytevector-input-port bstr))
  (syrup-read bstr-port #:unmarshallers unmarshallers))

