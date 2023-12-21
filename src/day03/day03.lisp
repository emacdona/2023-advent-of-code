;;;; day03.lisp

(in-package #:day03)

(defvar *input-file*
  (asdf:system-relative-pathname
   "advent-of-code-2023"
   "resources/day03/input"))

(defun make-grid ()
  (mapcar (lambda (line)
            (coerce line 'list))
          (get-input-lines)))

(defun make-stream (grid)
  (loop
    for row in grid
    for i = 0 then (+ 1 i)
    append
    (loop
      for col in row
      for j = 0 then (+ 1 j)
      collect (list i j col))))

(defclass number-region ()
    (digits
     top-left-coord
     lower-right-coord))

(defun find-symbols (grid)
  )
