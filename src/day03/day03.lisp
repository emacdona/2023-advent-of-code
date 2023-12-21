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
        #\NORTH_WEST_TRIANGLE-HEADED_ARROW_TO_BAR)

       ((and
         (top-boundary-p obj)
         (right-boundary-p obj))
        #\NORTH_EAST_TRIANGLE-HEADED_ARROW_TO_BAR)

       ((and
         (bottom-boundary-p obj)
         (left-boundary-p obj))
        #\SOUTH_WEST_TRIANGLE-HEADED_ARROW_TO_BAR)

       ((and
        (bottom-boundary-p obj)
        (right-boundary-p obj))
        #\SOUTH_EAST_TRIANGLE-HEADED_ARROW_TO_BAR)

       ((top-boundary-p obj) #\UPWARDS_TRIANGLE-HEADED_ARROW_TO_BAR)

       ((bottom-boundary-p obj) #\DOWNWARDS_TRIANGLE-HEADED_ARROW_TO_BAR)

       ((left-boundary-p obj) #\LEFTWARDS_TRIANGLE-HEADED_ARROW_TO_BAR)

       ((right-boundary-p obj) #\RIGHTWARDS_TRIANGLE-HEADED_ARROW_TO_BAR)

       (t #\DOTTED_SQUARE)))))
