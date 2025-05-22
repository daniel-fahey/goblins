;;; Copyright 2021 Christine Lemmer-Webber
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

(define-module (tests ocapn test-ids)
  #:use-module (goblins utils hashmap)
  #:use-module (goblins ocapn ids)
  #:use-module (srfi srfi-64))

(test-begin "test-ids")

(define ocapn-m1
  (make-ocapn-node
   'fake
   "4wy6gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
   (hashmap ("name" "test 1"))))

(define ocapn-m1*
  (make-ocapn-node
   'fake
   "4wy6gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
   (hashmap ("name" "test 2"))))

(define ocapn-m2
  (make-ocapn-node
   'fake
   "8upy8klbgvtxwopxz93oyx5rxtglasaphptdjbb0hqjfvsalsinc9p7g"
   (hashmap ("name" "test 3"))))

(define ocapn-sref1
  (make-ocapn-sturdyref ocapn-m1 #vu8(74 174 136 226 211 114 92 53 153 139 168 28 82 26 52 183 107 50 123 83 116 61 247 240 172 189 77 35 75 63 51 162)))

(test-assert
    "Verify ocapn-node? tests positive when given an ocapn-node"
  (ocapn-node? ocapn-m1))

;; ocapn-id->ocapn-node
(test-equal "ocapn-id->ocapn-node with an ocapn-node"
  ocapn-m1
  (ocapn-id->ocapn-node ocapn-m1))

(test-equal "ocapn-id->ocapn-node with an ocapn-studyref"
  ocapn-m1
  (ocapn-id->ocapn-node ocapn-sref1))

;; same-node-location?
(test-assert
    "same-node-location? with the same node, and same hints"
  (same-node-location? ocapn-m1 ocapn-m1))

(test-assert
    "same-node-location? with the same node, but different hints"
  (same-node-location? ocapn-m1 ocapn-m1*))

(test-assert
    "same-node-location? doesn't match with two different nodes"
  (not (same-node-location? ocapn-m1 ocapn-m2)))

;; Check string->ocapn-id
(test-equal "Verify string->ocapn-id produces the correct ocapn-node"
  ocapn-m1
  (string->ocapn-id "ocapn://4wy6gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd.fake?name=test%201"))

(test-equal "Verify string->ocapn-id produces the correct ocapn-studyref"
  ocapn-sref1
  (string->ocapn-id "ocapn://4wy6gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd.fake/s/Sq6I4tNyXDWZi6gcUho0t2sye1N0PffwrL1NI0s_M6I?name=test%201"))

;; Check ocapn-id->string
(test-equal "ocapn-id->string works for ocapn-node"
  "ocapn://4wy6gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd.fake?name=test%201"
  (ocapn-id->string ocapn-m1))

(test-equal "ocapn-id->string works for ocapn-studyref"
  "ocapn://4wy6gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd.fake/s/Sq6I4tNyXDWZi6gcUho0t2sye1N0PffwrL1NI0s_M6I?name=test%201"
  (ocapn-id->string ocapn-sref1))

(test-end "test-ids")
