;;;; day03.lisp

(in-package #:day03)

(defvar *input-file* "../../resources/day03/input")

(defun make-grid ()
  (mapcar (lambda (line)
            (coerce line 'list))
          (get-input-lines *input-file*)))

(defun find-numbers (grid)
  )

(defun find-symbols (grid)
  )
