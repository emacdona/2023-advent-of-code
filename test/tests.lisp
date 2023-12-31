;;;; tests.lisp

(in-package #:tests)

;; These tests will let us refactor our solutions with confidence!
;; To run them all from the REPL:
;; (in-package #:tests)
;; (run!)

(test day-01-answer-01
  "Test that Day 1, Problem 1 answer is still correct."
  (is
   (= 53080 (day-01-answer-01))))

(test day-01-answer-02
  "Test that Day 1, Problem 2 answer is still correct."
  (is
   (= 53268 (day-01-answer-02))))

(test day-02-answer-01
  "Test that Day 2, Problem 1 answer is still correct."
  (is
   (= 2679 (day-02-answer-01))))

(test day-02-answer-02
  "Test that Day 2, Problem 2 answer is still correct."
  (is
   (= 77607 (day-02-answer-02))))

(test day-03-answer-01
  "Test that Day 3, Problem 1 answer is still correct."
  (is
   (= 540131 (day-03-answer-01))))
