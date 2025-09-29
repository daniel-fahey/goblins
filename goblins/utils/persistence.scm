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

(define-module (goblins utils persistence)
  #:use-module (goblins abstract-types)
  #:use-module (goblins utils ghash)
  #:use-module (goblins utils hashmap)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-1)
  #:export (remove-orphaned-objects))

(define (remove-orphaned-objects portraits roots)
  "Returns a portrait data of the objects which appear in the graph starting from @var{roots}."

  (define reachable (make-hash-table))

  (define (visit-data data)
    ;; Contains a list of all the compound data structures which might be
    ;; reachable.
    (match data
      ;; Near refrs.
      (($ <tagged> 'near (slot)) (visit-object slot))
      ;; Lists
      ((left . right)
       (visit-data left)
       (visit-data right))
      ;; Vectors
      (($ <tagged> 'vec data)
       (visit-data data))
      ;; gsets
      ((? gset?)
       (gset-for-each visit-data data))
      ;; Hashmaps
      ((? hashmap?)
       (hashmap-for-each
        (lambda (key value)
          (visit-data key)
          (visit-data value))
        data))
      ;; tagged
      (($ <tagged> 'tagged payload)
       (visit-data payload))
      (_ (values))))

  (define (visit-object slot)
    (match (hash-ref reachable slot)
      (#f
       (match (hash-ref portraits slot)
         ((and portrait (_ _ _ data))
          (hash-set! reachable slot portrait)
          (visit-data data))))
      (_ (values))))

  ;; Start at the roots.
  (for-each visit-object roots)
  reachable)
