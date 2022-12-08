;; This source code is largely from guile's web uris module
;; Commit: ff7328df0
;; License: GNU Lesser General Public version 3 or later (LGPL v3+)

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
(define-module (goblins ocapn uri)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 control)
  #:use-module (web uri)
  #:export (string->uri))

(define digits "0123456789")
(define hex-digits "0123456789ABCDEFabcdef")
(define letters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
(define ipv4-regexp
  (make-regexp (string-append "^([" digits ".]+)$")))
(define ipv6-regexp
  (make-regexp (string-append "^([" hex-digits "]*:[" hex-digits ":.]+)$")))
(define registered-name-regexp
  (make-regexp (string-append "^([" letters digits "]+[.]?)+$")))

(define (valid-host? host)
  (cond
   ((regexp-exec ipv4-regexp host)
    (false-if-exception (inet-pton AF_INET host)))
   ((regexp-exec ipv6-regexp host)
    (false-if-exception (inet-pton AF_INET6 host)))
   (else (regexp-exec registered-name-regexp host))))

(define scheme-pat
  (string-append "[" letters "][" letters digits "+.-]*"))
(define authority-pat
  "[^/?#]*")
(define path-pat
  "[^?#]*")
(define query-pat
  "[^#]*")
(define fragment-pat
  ".*")
(define uri-pat
  (format #f "^((~a):)?(//~a)?(~a)(\\?(~a))?(#(~a))?$"
          scheme-pat authority-pat path-pat query-pat fragment-pat))
(define uri-regexp
  (make-regexp uri-pat))


(define userinfo-pat
  (string-append "[" letters digits "_.!~*'();:&=+$,-]+"))
(define host-pat
  (string-append "[" letters digits ".-]+"))
(define ipv6-host-pat
  (string-append "[" hex-digits ":.]+"))
(define port-pat
  (string-append "[" digits "]*"))
(define authority-regexp
  (make-regexp
   (format #f "^//((~a)@)?((~a)|(\\[(~a)\\]))(:(~a))?$"
           userinfo-pat host-pat ipv6-host-pat port-pat)))

(define (parse-authority authority fail)
  (if (equal? authority "//")
      ;; Allow empty authorities: file:///etc/hosts is a synonym of
      ;; file:/etc/hosts.
      (values #f #f #f)
      (let ((m (regexp-exec authority-regexp authority)))
        (if (and m (valid-host? (or (match:substring m 4)
                                    (match:substring m 6))))
            (values (match:substring m 2)
                    (or (match:substring m 4)
                        (match:substring m 6))
                    (let ((port (match:substring m 8)))
                      (and port (not (string-null? port))
                           (string->number port))))
            (fail)))))

(define make-uri
  (@@ (web uri) make-uri))

(define (string->uri-reference string)
  "Parse STRING into a URI-reference object.  Return ‘#f’ if the string
could not be parsed."
  (% (let ((m (regexp-exec uri-regexp string)))
       (unless m (abort))
       (let ((scheme (let ((str (match:substring m 2)))
                       (and str (string->symbol (string-downcase str)))))
             (authority (match:substring m 3))
             (path (match:substring m 4))
             (query (match:substring m 6))
             (fragment (match:substring m 8)))
         ;; The regular expression already ensures all of the validation
         ;; requirements for URI-references, except the one that the
         ;; first component of a relative-ref's path can't contain a
         ;; colon.
         (unless scheme
           (let ((colon (string-index path #\:)))
             (when (and colon (not (string-index path #\/ 0 colon)))
               (abort))))
         (call-with-values
             (lambda ()
               (if authority
                   (parse-authority authority abort)
                   (values #f #f #f)))
           (lambda (userinfo host port)
             (make-uri scheme userinfo host port path query fragment)))))
     (lambda (k)
       #f)))

(define (string->uri string)
  "Parse STRING into a URI object.  Return ‘#f’ if the string could not
be parsed.  Note that this procedure will require that the URI have a
scheme."
  (let ((uri-reference (string->uri-reference string)))
    (and (not (relative-ref? uri-reference))
         uri-reference)))
