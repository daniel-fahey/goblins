;;; Copyright 2025 Jessica Tallon
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

(use-modules (goblins)
             (goblins vat)
             (goblins ocapn netlayer unix-domain-socket)
             (goblins utils unix-domain-socket)
             (goblins utils unix-domain-socket-server)
             (goblins persistence-store syrup)
             (fibers conditions))


(define %default-socket-file-name "/tmp/goblins-unix-domain-socket-server.sock")
(define socket-file-name
  (or (getenv "GOBLINS_UDS_SERVER_FILENAME") %default-socket-file-name))
(define syrup-store-file-name (getenv "GOBLINS_UDS_SYRUP_STORE_FILENAME"))

(define-values (vat uds-server)
  (if syrup-store-file-name
      (spawn-persistent-vat
       unix-domain-socket-server-env
       (lambda ()
         (spawn ^unix-domain-socket-server))
       (make-syrup-store syrup-store-file-name))
      (let* ((vat (spawn-vat))
             (uds-server (with-vat vat (spawn ^unix-domain-socket-server))))
        (values vat uds-server))))

(define sock (make-unix-domain-socket))
(with-vat vat
  (define address (make-socket-address AF_UNIX socket-file-name))
  ($ uds-server sock address))

;; If ctrl+c is given stop the vat and remove the file
(define quit-condition (make-condition))
(sigaction SIGINT
  (lambda _
    ;; Call it again after setup, invokes the halt behavior
    (with-vat vat
      ($ uds-server))
    ;; Halt the vat so we should be done writing to the store.
    (vat-halt! vat)
    ;; Remove the UDS file
    (delete-file socket-file-name)
    (signal-condition! quit-condition)))
(wait quit-condition)
