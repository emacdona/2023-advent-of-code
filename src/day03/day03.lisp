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
    for row on grid
    for i = 0 then (+ 1 i)
    append
    (loop
      for col on (car row)
      for j = 0 then (+ 1 j)
      collect
      (make-instance
       'token
       :token-char (car col)
       :i i
       :j j
       :top-boundary-p (if (= i 0) t nil)
       :bottom-boundary-p (if (cdr row) nil t)
       :left-boundary-p (if (= j 0) t nil)
       :right-boundary-p (if (cdr col) nil t)))))

(defclass token ()
  ((char
    :initarg :token-char
    :reader token-char)
   (i
    :initarg :i
    :reader i)
   (j
    :initarg :j
    :reader j)
   (top-boundary-p
    :initarg :top-boundary-p
    :reader top-boundary-p)
   (bottom-boundary-p
    :initarg :bottom-boundary-p
    :reader bottom-boundary-p)
   (left-boundary-p
    :initarg :left-boundary-p
    :reader left-boundary-p)
   (right-boundary-p
    :initarg :right-boundary-p
    :reader right-boundary-p)))

(defmethod print-object ((obj token) out)
  (print-unreadable-object (obj out :type t)
    (format
     out "(~d,~d): '~a' ~a"
     (i obj)
     (j obj)
     (token-char obj)
     (cond
       ((and
        (top-boundary-p obj)
        (left-boundary-p obj))
        #\BOX_DRAWINGS_DOUBLE_DOWN_AND_RIGHT)

       ((and
         (top-boundary-p obj)
         (right-boundary-p obj))
        #\BOX_DRAWINGS_DOUBLE_DOWN_AND_LEFT)

       ((and
         (bottom-boundary-p obj)
         (left-boundary-p obj))
        #\BOX_DRAWINGS_DOUBLE_UP_AND_RIGHT)

       ((and
        (bottom-boundary-p obj)
        (right-boundary-p obj))
        #\BOX_DRAWINGS_DOUBLE_UP_AND_LEFT)

       ((top-boundary-p obj) #\BOX_DRAWINGS_DOUBLE_DOWN_AND_HORIZONTAL)

       ((bottom-boundary-p obj) #\BOX_DRAWINGS_DOUBLE_UP_AND_HORIZONTAL)

       ((left-boundary-p obj) #\BOX_DRAWINGS_DOUBLE_VERTICAL_AND_RIGHT)

       ((right-boundary-p obj) #\BOX_DRAWINGS_DOUBLE_VERTICAL_AND_LEFT)

       (t #\BOX_DRAWINGS_DOUBLE_VERTICAL_AND_HORIZONTAL)))))
