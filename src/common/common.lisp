;;;; common.lisp

(in-package #:common)

(defun transform-lines (filename transformer)
  (with-open-file (in filename)
    (loop for (line no-nl-p) = (multiple-value-list (read-line in nil nil))
          while line
          collect (funcall transformer line))))

;; As long as *input-file* is defined, you're good to go
(defmacro get-input-lines ()
  ;; https://stackoverflow.com/questions/44199651/exporting-anaphoric-macros-in-common-lisp-packages
  (let ((if (intern (symbol-name '*input-file*))))
    `(transform-lines ,if #'identity)))


