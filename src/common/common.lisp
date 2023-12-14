;;;; common.lisp

(in-package #:common)

(defun transform-lines (filename transformer)
  (with-open-file (in filename)
    (loop for (line no-nl-p) = (multiple-value-list (read-line in nil nil))
          while line
          collect (funcall transformer line))))

(defun show-input-lines (filename)
  (transform-lines filename #'identity))
