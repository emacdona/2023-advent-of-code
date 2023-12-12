;;;; advent-of-code-2023.lisp

(in-package #:advent-of-code-2023)

(defun transform-lines (filename transformer)
  (with-open-file (in filename)
    (loop for (line no-nl-p) = (multiple-value-list (read-line in nil nil))
          while line
          do (funcall transformer line))))


(defun show-input-lines ()
  (let ((filename "./resources/day01/input"))
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


