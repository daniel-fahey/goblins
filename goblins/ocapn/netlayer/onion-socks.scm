;;; Copyright 2021-2022 Christine Lemmer-Webber
;;; Copyright 2022 Jessica Tallon
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

(define-module (goblins ocapn netlayer onion-socks)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (goblins ocapn netlayer utils)
  #:use-module (fibers channels)
  #:export (onion-socks5-setup!))

(define (read-bytes p bytes-to-read)
  (if (<= bytes-to-read 0)
      (list)
      (cons (get-message p)
            (read-bytes p (- bytes-to-read 1)))))

;; Setups up a basic socks 5 connection
;; rfc1928
;; NB: This is untested code.
(define (onion-socks5-setup! sock address port)
  (define (read-and-expect-protocol-5)
    (match (get-u8 sock)
      (#x05 'ok)
      (anything-else
       (error (format #f "Wrong socks protocol number: ~a" anything-else)))))
  (define (read-and-expect-no-authentication)
    (match (get-u8 sock)
      (#x00 'ok)
      (anything-else
       (error
        (format #f "Unsupported authentication method: ~a" anything-else)))))

  (put-u8 sock #x05) ;; protocol 5
  (put-u8 sock #x01) ;; method 1.
  (put-u8 sock #x00) ;; no authentication.
  (read-and-expect-protocol-5)
  (read-and-expect-no-authentication)

  ;; Protocol 5, connect, <reserved>, RDNS
  (put-u8 sock #x05) ;; protocol 5
  (put-u8 sock #x01) ;; connect
  (put-u8 sock #x00) ;; reserved.
  (put-u8 sock #x03) ;; RDNS
  ;; All v3 onion addresses are 62 chars long.
  (put-u8 sock 62)
  ;; Write the onion address
  (put-bytevector sock (string->utf8 address))

  ;; Write out the port.
  (let ((port-bv (make-bytevector 2)))
    (bytevector-u16-set! port-bv 0 port (endianness big))
    (put-bytevector sock port-bv))

  (read-and-expect-protocol-5)
  (match (get-u8 sock)
    (#x00 'ok)
    (#x01 (error "General SOCKS server failure"))
    (#x02 (error "Connection not allowed by ruleset"))
    (#x03 (error "Network unreachable"))
    (#x04 (error "Connection refused"))
    (#x05 (error "TTL expired"))
    (#x06 (error "Command not supported"))
    (#x07 (error "Address type not supported"))
    (error-number
     (error (format #f "Unassigned socks error... (error: ~a)" error-number))))

  (match (get-u8 sock)
    ;; Domain
    (#x03
     (let* ((address-size (get-u8 sock)))
       (get-bytevector-n sock address-size)))

    ;; IPv4 (4: address + 2: port)
    (#x01 (get-bytevector-n sock (+ 4 2)))
    ;; IPv6 (16: address + 2: port)
    (#x04 (get-bytevector-n sock (+ 16 2))))
  'ok)
