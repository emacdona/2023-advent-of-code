;;;; advent-of-code-2023.lisp

(in-package #:advent-of-code-2023)

(defun transform-lines ()
  (with-open-file (in "./resources/day01/input")
    (loop for (line no-nl-p)
            = (multiple-value-list (read-line in nil nil))
          while line
          do (format t "~S~:[ <newline at end>~;~]~%"
                     line no-nl-p))))


