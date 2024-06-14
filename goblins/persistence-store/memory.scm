;;; Copyright 2024 Jessica Tallon
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

(define-module (goblins persistence-store memory)
  #:use-module (goblins core-types)
  #:use-module (goblins actor-lib methods)
  #:export (make-memory-store))

(define (make-memory-store)
  "Provides a simple in memory store"
  (define vat-aurie-id #f)
  (define stored-portraits #f)
  (define stored-roots #f)
  (define memory-read-proc
    (methods
     [(graph-and-slots)
      (values vat-aurie-id stored-portraits stored-roots)]
     [(object-portrait slot)
      (unless stored-portraits
        (error "No portrait data has been stored yet"))
      (hashq-ref stored-portraits slot)]))
      
  (define memory-save-proc
    (methods
     [(save-graph aurie-id portraits roots)
      (set! vat-aurie-id aurie-id)
      (set! stored-portraits portraits)
      (set! stored-roots roots)]
     [(save-delta delta-portraits)
      (unless (and stored-portraits stored-roots)
        (error "No portrait data has been stored yet"))
      (hash-for-each
       (lambda (key value)
         (hashq-set! stored-portraits key value))
       delta-portraits)]))
  (make-persistence-store memory-read-proc memory-save-proc))
