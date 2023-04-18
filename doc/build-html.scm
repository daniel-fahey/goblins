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

;; Texinfo's 'makeinfo --html' command generates disappointing HTML.
;; To deal with it, we post-process the HTML files that it generates
;; to add in syntax highlighting and a better stylesheet.
(use-modules (htmlprag)
             (ice-9 ftw)
             (ice-9 match)
             (srfi srfi-1)
             (syntax-highlight)
             (syntax-highlight scheme))

(define %html-dir "goblins.html")
(define %image-dir (string-append %html-dir "/images"))
(define %css-file "goblins.css")

;; Work within the context of the docs directory.
(chdir (dirname (current-filename)))

;; Generate the docs with makeinfo.
(unless (zero? (system* "makeinfo" "--html" "-o" %html-dir "goblins.texi"))
  (error "failed to build manual"))

;; Copy our CSS file to the build artifact directory.
(copy-file %css-file (string-append %html-dir "/" %css-file))

;; Gather up all the HTML files that were generated.
(define html-files
  (filter-map (lambda (f)
                (and (string-suffix? ".html" f)
                     (string-append %html-dir "/" f)))
              (scandir %html-dir)))

;; Post-process a single document.
(define (prettify-sxml sxml)
  (match sxml
    ;; Add our stylesheet to the <head> section...
    (('head nodes ...)
     `(head ,@(map prettify-sxml nodes)
            (link (@ (rel "stylesheet")
                     (href "goblins.css")))))
    ;; ...and remove the default style!
    (('style _ ...) "")
    ;; Highlight Scheme code.
    (('pre ('@ ('class "lisp")) lines ...)
     (let ((highlights (highlight lex-scheme (string-concatenate lines))))
       `(pre (@ (class "lisp"))
             ,@(highlights->sxml highlights))))
    ;; Leaf nodes.
    ((or (? symbol?) (? string?)) sxml)
    ;; Recursively descend through SXML nodes.  Requires two cases:
    ;; One for nodes with attributes, and one for nodes without.
    (((? symbol? tag) ('@ attrs ...) nodes ...)
     (cons* tag
            (cons '@ attrs)
            (map prettify-sxml nodes)))
    (((? symbol? tag) nodes ...)
     (cons tag (map prettify-sxml nodes)))))

;; Parse HTML strictly.
(%strict-tokenizer? #t)

;; Apply post-processing to all HTML files, overwriting their original
;; contents.
(for-each (lambda (f)
            (let ((sxml (call-with-input-file f html->sxml)))
              (call-with-output-file f
                (lambda (port)
                  (write-sxml-html (prettify-sxml sxml) port)))))
          html-files)

;; Copy images.
(unless (file-exists? %image-dir)
  (mkdir %image-dir))
(for-each (match-lambda
            ((or "." "..")
             #f)
            (file-name
             (copy-file (string-append "images/" file-name)
                        (string-append %image-dir "/" file-name))))
          (scandir "images"))
