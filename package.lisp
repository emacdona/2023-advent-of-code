;;;; package.lisp

(defpackage #:common
  (:use #:cl)
  (:export #:transform-lines
           #:get-input-lines))

(defpackage #:day01
  (:use #:cl #:common)
  (:export
   #:day-01-answer-01
   #:day-01-answer-02))

(defpackage #:day02
  (:use #:cl #:common)
  (:export
   #:day-02-answer-01
   #:day-02-answer-02))

(defpackage #:day03
  (:use #:cl #:common)
  (:export
   #:day-03-answer-01))

(defpackage #:tests
  (:use #:cl
        #:fiveam
        #:day01
        #:day02
        #:day03))
