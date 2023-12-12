;;;; advent-of-code-2023.lisp

(in-package #:advent-of-code-2023)

(defvar *input-file* "./resources/day01/input")

(defun transform-lines (filename transformer)
  (with-open-file (in filename)
    (loop for (line no-nl-p) = (multiple-value-list (read-line in nil nil))
          while line
          collect (funcall transformer line))))

(defun show-input-lines ()
  (let ((filename *input-file*))
    (transform-lines filename
                     (lambda (line) (format t "~S~%" line)))))

;;; Given a string, find the first digit.
;;; If no digits found, return "default"
(defun first-digit (string &optional (default 0))
  (let ((_chars (coerce string 'list)))
    (labels ((find-first-digit (chars)
               (let* ((c (car chars))
                      (rest (cdr chars))
                      (digit
                        (if (characterp c)
                            (digit-char-p c)
                            nil)))
                 (cond
                   (digit digit)
                   (rest (find-first-digit rest))
                   ('t default)))))
      (find-first-digit _chars))))

;;; Given a string, find the last digit.
;;; If no digits found, return "default"
(defun last-digit (string &optional (default 0))
  (first-digit (reverse string) default))

(defun reduce-line (line)
  (+ (* 10 (first-digit line))
     (last-digit line)))

(defun day-01-answer-01 ()
  ;; 53080
  (apply #'+ (transform-lines *input-file* #'reduce-line)))

(defvar *word-to-digit*
  '(("one" . 1)
    ("two" . 2)
    ("three" . 3)
    ("four" . 4)
    ("five" . 5)
    ("six" . 6)
    ("seven" . 7)
    ("eight" . 8)
    ("nine" . 9)))

(defmacro regex (string forward)
  `(cl-ppcre:regex-replace-all
    '(:register
      (:alternation
       ,@(loop for pair in *word-to-digit*
               collect (if forward (car pair) (reverse (car pair))))))
    (if ,forward ,string (reverse ,string))
    #'(lambda (match &rest registers)
        (write-to-string
         (cdr
          (assoc (if ,forward (car registers) (reverse (car registers)))
                 ',*word-to-digit*
                 :test #'string=))))
    :simple-calls t))

(defun day-01-answer-02 ()
  ;; 53255 - too low
  ;; "twone345twone" => 2ne345tw1 (think about that)
  ;; 53268 <= correct
  (apply #'+ (transform-lines *input-file*
                              #'(lambda (line)
                                  (+ (* 10 (first-digit (regex line 't)))
                                     (first-digit (regex line nil)))))))
