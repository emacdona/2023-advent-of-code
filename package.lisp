;;;; package.lisp

(defpackage #:common
  (:use #:cl)
  (:export #:transform-lines
           #:show-input-lines))

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
  (:use #:cl #:common))

(defpackage #:tests
  (:use #:cl
        #:fiveam
        #:day01
        #:day02))
