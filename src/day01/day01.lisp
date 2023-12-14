;;;; day01.lisp

(in-package #:day01)

(defvar *input-file* "../../resources/day01/input")

;;; Given a string, find the first digit.
;;; If no digits found, return "default"
(defun first-digit (string &optional (default 0))
  (let ((chars (coerce string 'list)))
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
                   (t default)))))
      (find-first-digit chars))))

;;; Given a string, find the last digit.
;;; If no digits found, return "default"
(defun last-digit (string &optional (default 0))
  (first-digit (reverse string) default))

(defun reduce-line-1 (line)
  (+ (* 10 (first-digit line))
     (last-digit line)))

(defun day-01-answer-01 ()
  ;; 53080
  (apply #'+ (transform-lines *input-file* #'reduce-line-1)))

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

;; Nice to have this separate, so that you can look at what it does via the REPL
;; Also, try this from the REPL to shed some light on this:
;;   (cl-ppcre:parse-string "(one|two|three)")
(defun number-words-regex (&key f (words-assoc *word-to-digit*))
  `(:register
    (:alternation
     ,@(loop for pair in words-assoc
             collect (funcall f (car pair))))))

(defun convert-words-to-digits (string &key (forward t) (words-assoc *word-to-digit*))
  (let* ((f (if forward #'identity #'reverse))
         (result
           (cl-ppcre:regex-replace-all
            (number-words-regex :f f :words-assoc words-assoc)
            (funcall f string)
            #'(lambda (&rest match-and-registers)
                (write-to-string
                 (cdr
                  (assoc (funcall f (cadr match-and-registers))
                         words-assoc
                         :test #'string=))))
            :simple-calls t)))
    (funcall f result)))

(defun reduce-line-2 (line)
  (let ((forward (convert-words-to-digits line))
        (backward (convert-words-to-digits line :forward nil)))
    (+ (* 10 (first-digit forward))
       (last-digit backward))))

(defun day-01-answer-02 ()
  ;; HINT: "twone345twone" => 2ne345tw1
  ;; 53268
  (apply #'+ (transform-lines *input-file* #'reduce-line-2)))
