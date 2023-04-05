;;; Copyright 2023 David Thompson
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

(define-module (goblins utils graphviz)
  #:use-module (goblins config)
  #:use-module (ice-9 match)
  #:export (sdot->dot
            run-dot))

(define* (sdot->dot exp port #:optional (depth 0))
  "Serialize EXP to PORT in Graphviz DOT format."
  (define (indent depth port)
    (when (> depth 0)
      (display (make-string (* depth 2) #\space) port)))
  (define (dot-escape str)
    (list->string
     (string-fold-right (lambda (c memo)
                          (if (eqv? c #\")
                              (cons* #\\ #\" memo)
                              (cons c memo)))
                        '() str)))
  (define (emit-attrs attrs port)
    (match attrs
      (('@ ((? symbol? keys) (? string? vals)) ...)
       (for-each (lambda (key val)
                   (format port "~a=\"~a\"," key (dot-escape val)))
                 keys vals))))
  (match exp
    (('edge from to)
     (indent depth port)
     (format port "~a -> ~a;\n" from to))
    (('edge from to attrs)
     (indent depth port)
     (format port "~a -> ~a[" from to)
     (emit-attrs attrs port)
     (format port "];\n"))
    (('node name)
     (indent depth port)
     (format port "~a;\n" name))
    (((or 'node 'attr) name attrs)
     (indent depth port)
     (format port "~a[" name)
     (emit-attrs attrs port)
     (format port "];\n"))
    (((and (or 'digraph 'subgraph) type) name exps ...)
     (indent depth port)
     (format port "~a ~a {\n" type name)
     (for-each (lambda (exp*)
                 (sdot->dot exp* port (+ depth 1)))
               exps)
     (indent depth port)
     (format port "}\n"))))

(define (run-dot . args)
  "Run graphviz 'dot' program with ARGS, a list of string arguments and
flags, and return the exit status."
  (apply system* %dot args))
