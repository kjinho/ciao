(defpackage ciao/tests/main
  (:use :cl
        :ciao
        :rove))
(in-package :ciao/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :brief-tools)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
