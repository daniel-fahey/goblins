;;; (C) 2020-2022 Christine Lemmer-Webber
;;; Licensed under Apache v2

;; Pulled from:
;;   https://github.com/ocapn/syrup/blob/master/impls/guile/syrup.scm
;; Eventually we should get out a separate package for just guile-syrup
;; but in the meanwhile this simplifies our life a little bit.

(define-module (goblins contrib syrup)
  #:use-module (srfi srfi-1)          ; lists
  #:use-module (srfi srfi-9)          ; records
  #:use-module (srfi srfi-9 gnu)      ; record extensions
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 format)
  #:use-module (goblins abstract-types)
  #:use-module (goblins utils base32)
  #:use-module (goblins utils hashmap)
  #:use-module (goblins utils ghash)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 textual-ports)

  #:export (;;; The main procedures
            ;;; -------------------
            syrup-encode
            syrup-decode
            syrup-read
            syrup-write

            ;; pseudosingles (pretend to be a single precision float)
            make-pseudosingle pseudosingle?
            psuedosingle->float

            make-marshallers
            define-syrup-record-type

            ;;; For jsyrup
            ;;; ----------
            jsyrup-write))

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

(define* (write-as-netstring! port bstr #:key [joiner colon-bv])
  (let ((bstr-len (bytevector-length bstr)))
    (put-bytevector port (string->utf8 (number->string bstr-len)))
    (put-bytevector port joiner)
    (put-bytevector port bstr)))

;; alias for simplicity
(define bytes string->utf8)

(define zero-bv
  (bytes "0+"))

(define (bytes<? bstr1 bstr2)
  (define bstr1-len
    (bytevector-length bstr1))
  (define bstr2-len
    (bytevector-length bstr2))
  (let lp ([pos 0])
    (cond
     ;; we've reached the end of both and they're the same bytestring
     ;; but this isn't <?
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
(define* (syrup-write obj out-port #:key [marshallers '()])
  (define (make-key-cons key)
    (cons (syrup-encode key #:marshallers marshallers)
          key))
  (define (build-encode-hash hash-ref hash-fold)
    (lambda (obj port)
      (let* ([keys-and-encoded
              (hash-fold
               (lambda (key _val prev)
                 (cons (make-key-cons key) prev))
               '()
               obj)]
             [sorted-keys-and-encoded
              (sort keys-and-encoded
                    (match-lambda*
                      [((encoded1 . _k1)
                        (encoded2 . _k2))
                       (bytes<? encoded1 encoded2)]))])
        (put-bytevector port curly-left-bv)
        (for-each
         (match-lambda
           [(enc-key . key)
            (let* ([val (hash-ref obj key)])
              (put-bytevector port enc-key)
              (syrup-write val port #:marshallers marshallers))])
         sorted-keys-and-encoded)
        (put-bytevector port curly-right-bv))))

  (define write-hash!
    (build-encode-hash hash-ref hash-fold))
  (define write-vhash!
    (build-encode-hash
     ;; we shouldn't need a not-found case here
     (lambda (vh key)
       (match (vhash-assoc key vh)
         [(_ . val) val]))
     vhash-fold))
  (define write-ghash!
    (build-encode-hash hashmap-ref hashmap-fold))
  (define (output-tagged! port obj)
    (put-bytevector port anglebrac-left-bv)
    (encode (tagged-label obj) #:port port)
    (for-each
     (lambda (arg)
       (encode arg #:port port))
     (tagged-data obj))
    (put-bytevector port anglebrac-right-bv))
  (define* (encode obj #:key [port #f])
    (match obj
      ;; Bytes are like <bytes-len>:<bytes>
      [(? bytevector?)
       (write-as-netstring! port obj)]
      [0 (put-bytevector port zero-bv)]
      ;; Integers are like <integer>+ or <integer>-
      [(? exact-integer?)
       (let* ((pos? (positive? obj))
              (number-to-output (if pos? obj (* obj -1)))
              (sign-char (if pos? plus-bv minus-bv))
              (encoded-number (string->utf8 (number->string number-to-output))))
         (put-bytevector port encoded-number)
         (put-bytevector port sign-char))]
      ;; Lists are like [<item1><item2><item3>]
      [(or (? pair?) '())
       (put-bytevector port squarebrac-left-bv)
       (for-each
        (lambda (item)
          (encode item #:port port))
        obj)
       (put-bytevector port squarebrac-right-bv)]
      ;; Dictionaries are like {<key1><val1><key2><val2>}
      ;; We sort by the key being fully encoded.
      [(? hash-table?)
       (write-hash! obj port)]
      [(? hashmap?)
       (write-ghash! obj port)]
      ;; Strings are like <encoded-bytes-len>"<utf8-encoded>
      [(? string?)
       (let ((encoded-string (string->utf8 obj)))
         (write-as-netstring! port encoded-string #:joiner doublequote-bv))]
      ;; Symbols are like <encoded-bytes-len>'<utf8-encoded>
      [(? symbol?)
       (let ((encoded-symbol (string->utf8 (symbol->string obj))))
         (write-as-netstring! port encoded-symbol #:joiner singlequote-bv))]
      ;; Single flonum floats are like F<big-endian-encoded-single-float>
      [(? pseudosingle?)
       (let ([bv (make-bytevector 4)])
         (bytevector-ieee-single-set! bv 0 obj (endianness big))
         (put-bytevector port F-bv)
         (put-bytevector port bv))]
      ;; Double flonum floats are like D<big-endian-encoded-double-float>
      [(and (? number?) (? inexact?) (? real?))
       (let ([bv (make-bytevector 8)])
         (bytevector-ieee-double-set! bv 0 obj (endianness big))
         (put-bytevector port D-bv)
         (put-bytevector port bv))]
      ;; Records are like <<tag><arg1><arg2>> but with the outer <> for realsies
      [(? tagged?) (output-tagged! port obj) ]
      ;; #t is t, #f is f
      [#t (put-bytevector port t-bv)]
      [#f (put-bytevector port f-bv)]
      ;; Sets are like #<item1><item2><item3>$
      [(? gset?)
       (let* ([encoded-items
               (gset-fold
                (lambda (item prev)
                  (cons (syrup-encode item #:marshallers marshallers)
                        prev))
                '() obj)]
              [sorted-items
               (sort encoded-items
                     bytes<?)])
         (put-bytevector port hash-bv)
         (for-each
          (lambda (sorted-item)
            (put-bytevector port sorted-item))
          sorted-items)
         (put-bytevector port dollar-bv))]
      [_
       (call/ec
        (lambda (return)
          (for-each (match-lambda
                      ((handles-it? . translate)
                       (when (handles-it? obj)
                         (let ((translated (translate obj)))
                           (if (record? translated)
                               (return (encode translated #:port port))
                               (error 'syrup-marshaller-returned-unsupported-type))))))
                    marshallers)
          (error "Unsupported Syrup type:" obj)))]))
  (encode obj #:port out-port))

(define* (syrup-encode obj #:key (marshallers '()))
  (call-with-output-bytevector
   (lambda (port)
     (syrup-write obj port #:marshallers marshallers))))

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
                  (string->symbol (utf8->string bstr))]
                 ['str
                  (utf8->string bstr)])]))]
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
                 (lp (hashmap-set ht key val)))]))]
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
               (make-tagged label args))))]
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
          (let lp ([s (make-gset)])
            (match (_peek-char)
              [#\$
               (read-byte in-port)
               s]
              [_
               (lp (gset-add s (read-next)))]))]
         [_
          (error 'syrup-invalid-char "Unexpected character at position ~a: ~a"
                 (file-position in-port)
                 (_peek-char))]))
     (read-next))))

(define* (syrup-decode bstr #:key (unmarshallers '()))
  (define bstr-port
    (open-bytevector-input-port bstr))
  (syrup-read bstr-port #:unmarshallers unmarshallers))

(define (make-marshallers label predicate ctor serialize)
  "Provides two values a marshaller and unmarshaller for a given record"
  (define (unmarshall-predicate a-label)
    (eq? label a-label))

  (values (cons predicate (lambda (obj)
                            (serialize label obj)))
          (cons unmarshall-predicate ctor)))

(define-syntax-rule (define-syrup-record-type name (ctor arg ...) pred
                      label marshall unmarshall
                      fields ...)
  (begin
    (define-record-type name
      (ctor arg ...)
      pred
      fields ...)
    (define-values (marshall unmarshall)
      (let ((serialize (lambda (obj-label obj)
                         (match obj
                           [($ name arg ...)
                            (make-tagged* obj-label arg ...)]))))
        (make-marshallers 'label pred ctor serialize)))))


;;; Jsyrup writer
;;; =============

;; TODO: Add indentation
(define* (jsyrup-write obj #:optional (op (current-output-port))
                       #:key [marshallers '()]
                       (hash? hashmap?)
                       (hash-fold hashmap-fold)
                       (set? gset?)
                       (set-fold gset-fold)
                       (pretty-print? #t))
  (define (write-obj obj indent)
    (define* (indent-and-newline #:optional (indent indent))
      (when pretty-print?
        (newline op)
        (do ((i 0 (1+ i)))
            ((= i indent))
          (put-string op "  "))))
    (match obj
      ;; Bytes are like |<base32>|
      [(? bytevector?)
       (put-char op #\|)
       (base32-encode obj #:out-port op)
       (put-char op #\|)]
      ;; Numbers are like 3.14 (floats) or 3 or -4 (ints)
      [(? number?)
       (if (exact? obj)
           (if (integer? obj)
               (display obj op)
               (error "Fractional numbers not supported in Syrup:" obj))
           (if (real? obj)
               (display obj op)
               (error "Imaginary numbers not supported in Syrup:" obj)))]
      ;; Lists are like [<item1>, <item2>, <item3>]
      ['() (put-string op "[]")]
      [(? pair?)
       (put-char op #\[)
       (let lp ((lst obj))
         (match lst
           (()
            (indent-and-newline)
            (put-char op #\]))
           ((item)
            (indent-and-newline (1+ indent))
            (write-obj item (1+ indent))
            (indent-and-newline)
            (put-char op #\]))
           ((item rest ...)
            (indent-and-newline (1+ indent))
            (write-obj item (1+ indent))
            (put-string op ", ")
            (lp rest))))]
      ;; Dictionaries are like {<key1>: <val1>, <key2>: <val2>}
      ;; We sort by the key being fully encoded.
      [(? hash? ht)
       (put-string op "{")
       (hash-fold
        (lambda (key val first?)
          (unless first?
            (put-string op (if pretty-print? "," ", ")))
          (indent-and-newline (1+ indent))
          (write-obj key (1+ indent))
          (put-string op ": ")
          (write-obj val (1+ indent))
          #f)  ; no longer first
        #t     ; for the first run
        ht)
       (indent-and-newline)
       (put-string op "}")]

      ;; Strings are like "foo"
      [(? string? str)
       (define (escape-char char)
         (display (match char
                    (#\" "\\\"")
                    (#\\ "\\\\")
                    (#\/ "\\/")
                    (#\backspace "\\b")
                    (#\page "\\f")
                    (#\newline "\\n")
                    (#\return "\\r")
                    (#\tab "\\t")
                    (_ char))
                  op))
       (put-char op #\")
       (string-for-each escape-char str)
       (put-char op #\")]
      ;; Symbols are like 'foo'
      [(? symbol? sym)
       (define (escape-char char)
         (display (match char
                    (#\` "\\`")
                    (#\\ "\\\\")
                    (#\/ "\\/")
                    (#\backspace "\\b")
                    (#\page "\\f")
                    (#\newline "\\n")
                    (#\return "\\r")
                    (#\tab "\\t")
                    (_ char))
                  op))
       (put-char op #\`)
       (string-for-each escape-char (symbol->string sym))
       (put-char op #\`)]
      ;; Records are like <<tag> <arg1>, <arg2>> but with the outer <> for realsies
      [(? tagged?)
       (put-char op #\<)
       (write-obj (tagged-label obj) indent)
       (match (tagged-data obj)
         (() (values))              ; tagged record with no data
         ((first . rest)            ; unroll, putting space before first arg
          (put-char op #\space)
          (write-obj first indent)
          (let lp ((items rest))    ; unroll rest of args
            (match items
              (() (values))
              ((item . rest)
               (put-string op ", ")
               (write-obj item indent)
               (lp rest))))))
       (put-char op #\>)]
      ;; true is true, false is false
      [#t (put-string op "true")]
      [#f (put-string op "false")]
      ;; Sets are like (<item1>, <item2>, <item3>)
      [(? set?)
       (put-char op #\()
       (let ((first? #t))
         (set-fold (lambda (item first?)
                     (unless first?
                       (put-string op (if pretty-print? "," ", ")))
                     (indent-and-newline (1+ indent))
                     (write-obj item (1+ indent))
                     #f)   ; no longer first
                   #t      ; for the first run
                   obj))
       (indent-and-newline)
       (put-char op #\))]
      [_
       (let lp ((marshallers marshallers))
         (match marshallers
           ('()  ; nothing found
            (error "Unsupported Syrup type:" obj))
           (((handles-it? . translate) . rest-marshallers)
            (cond
             ((handles-it? obj)
              (let ((translated (translate obj)))
                (if (record? translated)
                    (write-obj translated indent)    ; done
                    (error 'syrup-marshaller-returned-unsupported-type))))
             (else
              (lp rest-marshallers))))))]))
  (write-obj obj 0)
  (when pretty-print? (newline op)))


