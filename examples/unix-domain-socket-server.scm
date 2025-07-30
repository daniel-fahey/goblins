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
             (goblins ocapn netlayer unix-domain-socket)
             (goblins utils unix-domain-socket)
             (goblins utils unix-domain-socket-server)
             (goblins persistence-store syrup)
             (fibers conditions))

(define filename "/tmp/the-goblins-god.sock")
(define-values (vat uds-server)
  (spawn-persistent-vat
   unix-domain-socket-server-env
   (lambda ()
     (spawn ^unix-domain-socket-server))
   (make-syrup-store "unix-domain-socket-server.syrup")))

(with-vat vat
  (define sock (make-unix-domain-socket))
  (define address (make-socket-address AF_UNIX filename))
  ($ uds-server sock address))

(define forever (make-condition))
(wait forever)
