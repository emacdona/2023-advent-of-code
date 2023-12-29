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

(defclass point ()
  ((i
    :initarg :i
    :reader i)
   (j
    :initarg :j
    :reader j)))

(defmethod print-object ((obj point) out)
  (print-unreadable-object (obj out :type t)
    (format
     out "(~d, ~d)"
     (i obj)
     (j obj))))

(defclass token ()
  ((char
    :initarg :token-char
    :reader token-char)

   ;; initialized with keywords :i and :j via initialize-instance:after
   (coordinate :reader coordinate)

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

(defmethod initialize-instance :after ((obj token) &key i j)
  (setf (slot-value obj 'coordinate)
        (make-instance 'point :i i :j j)))

(defmethod print-object ((obj token) out)
  (print-unreadable-object (obj out :type t)
    (format
     out "~a: '~a' ~a"
     (coordinate obj)
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

(defgeneric top-left (obj))

(defmethod top-left ((obj token))
  (with-slots (coordinate top-boundary-p left-boundary-p) obj
    (with-slots (i j) coordinate
      (let ((new-i (- i 1))
            (new-j (- j 1)))
        (cond
          ((and top-boundary-p left-boundary-p)
           coordinate)
          (top-boundary-p
           (make-instance 'point :i i :j new-j))
          (left-boundary-p
           (make-instance 'point :i new-i :j j))
          (t
           (make-instance 'point :i new-i :j new-j)))))))

(defgeneric bottom-right (obj))

(defmethod bottom-right ((obj token))
  (with-slots (coordinate bottom-boundary-p right-boundary-p) obj
    (with-slots (i j) coordinate
      (let ((new-i (+ i 1))
            (new-j (+ j 1)))
        (cond
          ((and bottom-boundary-p right-boundary-p)
           coordinate)
          (bottom-boundary-p
           (make-instance 'point :i i :j new-j))
          (right-boundary-p
           (make-instance 'point :i new-i :j j))
          (t
           (make-instance 'point :i new-i :j new-j)))))))

;; A number token has a value and a region. The region is used to determine whether it
;; overlaps with a character.
(defclass number-token ()
  ((region-top-left
    :initarg region-top-left
    :accessor region-top-left
    :initform nil)
   (region-bottom-right
    :initarg region-bottom-right
    :accessor region-bottom-right
    :initform nil)
   (value
    :initarg value
    :initform 0
    :accessor value)))

(defmethod print-object ((obj number-token) out)
  (print-unreadable-object (obj out :type t)
    (with-slots (region-top-left
                 region-bottom-right
                 value) obj
    (format
     out "top-left: ~a; bottom-right: ~a; value: ~d"
     region-top-left
     region-bottom-right
     value))))

(defmethod initialize-instance :after ((obj number-token) &key digit)
  (add-digit obj digit))

(defgeneric add-digit (instance digit))

(defmethod add-digit ((instance number-token) digit)
  (with-slots (value) instance
    (setf
     value
     (+
      (* 10 value)
      (digit-char-p digit)))))

(defgeneric accum-digit-token (instance token))

(defmethod accum-digit-token ((instance number-token) (token token))
  (let* ((token-top-left (top-left token))
         (token-bottom-right (bottom-right token))
         (token-top-left-i (i token-top-left))
         (token-top-left-j (j token-top-left))
         (token-bottom-right-i (i token-bottom-right))
         (token-bottom-right-j (j token-bottom-right)))
    (with-slots (region-top-left region-bottom-right) instance
      (with-slots ((region-top-left-i i) (region-top-left-j j))
          region-top-left
        (with-slots ((region-bottom-right-i i) (region-bottom-right-j j))
            region-bottom-right
          (add-digit instance (token-char token))
          (setf region-top-left
                (make-instance
                 'point
                 :i (cond
                      (region-top-left
                       (min token-top-left-i region-top-left-i))
                      (token-top-left-i))
                 :j (cond
                      (region-top-left
                       (min token-top-left-j region-top-left-j))
                      (token-top-left-j))))
          (setf region-bottom-right
                (make-instance
                 'point
                 :i (cond
                      (region-bottom-right
                       (max token-bottom-right-i region-bottom-right-i))
                      (token-bottom-right-i))
                 :j (cond
                      (region-bottom-right
                       (max token-bottom-right-j region-bottom-right-j))
                      (token-bottom-right-j))))
          instance)))))

(defvar *stream* (make-stream (make-grid)))

(defun parse (stream)
  (labels
      ((my-parse (stream accum &optional (symbols '()) (numbers '()))
         (let* ((token (car stream)))
           (with-slots (token-char right-boundary-p) token
             (cond
               ((nullp accum)
                (cond
                  ((digit-char-p token-char)
                   (my-parse
                    (cdr stream)
                    (make-instance 'number-token :digit token-char)
                    symbols
                    numbers)
                   )
                  ((not (eq #\. token-char))
                   )
                  (t
                   )
                  )
                )
               (t

                )
               )
             ))))
    ))
