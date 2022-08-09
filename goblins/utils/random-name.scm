;;; Copyright 2022 Christine Lemmer-Webber
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

(define-module (goblins utils random-name)
  #:export (random-name))

;; Probably not to be used for security-critical purposes.
;; This module is a quick kludge... we might want to do one that does ensure
;; security-safe randomness.  Doesn't account for hamming distance
;; considerations either, so these aren't meant to be strongly distinguishable
;; by a user.

(define random-name-chars
  #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
    #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

(define random-name-chars-len (vector-length random-name-chars))

(define (random-char n)
  (vector-ref random-name-chars n))

(define %random-state (make-parameter #f))

(define (get-or-make-random-state)
  (or (%random-state)
      (let ((new-state (random-state-from-platform)))
        (%random-state new-state)
        new-state)))

(define* (random-name len
                      #:key (random-state
                             (get-or-make-random-state)))
  "Generate a random name of LEN characters long.

#:random-state, if provided, is a random state such as generated
from (random-state-from-platform).

This procedure can be used for names which avoid collision but
don't have serious security considerations."
  (do ((n 0 (1+ n))             ; iterate to len
       (chars '()               ; set of characters
              (cons (random-char
                     (random random-name-chars-len random-state))
                    chars)))
      ((= n len)                ; end on meeting length
       (list->string chars))))  ; return string of random characters
