;;; Copyright 2020-2021 Christine Lemmer-Webber
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

(define-module (tests utils bytes-stuff)
  #:use-module (goblins utils bytes-stuff)
  #:use-module (srfi srfi-64))

(test-begin "test-bytes-stuff")

(test-assert "same bytestring isn't less"
  (not (bytes<? (bytes "meep")
                (bytes "meep"))))
(test-assert "greater bytestring of same length isn't less"
  (not (bytes<? (bytes "meep")
                (bytes "beep"))))
(test-assert "lesser bytestring of same length is less"
  (bytes<? (bytes "beep")
           (bytes "meep")))
(test-assert "greater bytestring of same length isn't less 2"
  (not (bytes<? (bytes "meep")
                (bytes "meeb"))))
(test-assert "lesser bytestring of same length is less 2"
  (bytes<? (bytes "meeb")
           (bytes "meep")))
(test-assert "shorter bytestring is less"
  (bytes<? (bytes "meep")
           (bytes "meeple")))
(test-assert "longer bytestring is greater"
  (not (bytes<? (bytes "meeple")
                (bytes "meep"))))

(test-end "test-bytes-stuff")
