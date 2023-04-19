;;; Copyright 2020 Christine Lemmer-Webber
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

;; An extremely simple mint, straight from
;;   http://erights.org/elib/capability/ode/index.html
(define-module (goblins actor-lib simple-mint)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins core)
  #:use-module (goblins utils simple-sealers)
  #:export (^mint
            withdraw))

(define (^mint _bcom)
  "Construct a mint to generate purses associated with the mint's
token.

Mint Methods:
`new-purse initial-balance': Spawn and return a new Purse containing
INITIAL-BALANCE tokens.

Purse Methods:
`get-balance': Return the amount of tokens in the Purse.
`sprout': Spawn and return a new Purse with 0 tokens.
`deposit amount src': Add AMOUNT tokens to the Purse from the Purse SRC.
`get-decr': Seal and return a procedure accepting a single argument, the
number of tokens to subtract from this Purse."
  (define-values (decr-seal decr-unseal _decr-sealed?)
    (make-sealer-triplet))
  (define (^purse _bcom initial-balance)
    (define balance (spawn ^cell initial-balance))
    (define (decr amount)
      (unless (and (integer? amount) (>= amount 0) (<= amount ($ balance)))
        (error 'mint-error "invalid decrement amount" amount))
      ($ balance (- ($ balance) amount)))
    (methods
     ((get-balance)
      ($ balance))
     ((sprout)
      (spawn ^purse 0))
     ((deposit amount src)
      (unless (and (integer? amount) (>= amount 0))
        (error 'mint-error "invalid deposit amount" amount))
      ((decr-unseal ($ src 'get-decr)) amount)
      ($ balance (+ ($ balance) amount)))
     ((get-decr)
      (decr-seal decr))))
  (methods
   ((new-purse initial-balance)
    (if (and (integer? initial-balance) (>= initial-balance 0))
        (spawn ^purse initial-balance)
        (error 'mint-error "invalid initial purse balance" initial-balance)))))

(define (withdraw amount from-purse)
  "Return a new purse containing AMOUNT that has been withdrawn from
FROM-PURSE.

Type: Number Purse -> Purse"
  (let ((new-purse ($ from-purse 'sprout)))
    ($ new-purse 'deposit amount from-purse)
    new-purse))
