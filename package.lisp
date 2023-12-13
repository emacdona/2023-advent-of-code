;;;; package.lisp

(defpackage #:common
  (:use #:cl)
  (:export #:transform-lines))

(defpackage #:day01
  (:use #:cl #:common))

(defpackage #:day02
  (:use #:cl #:common))
