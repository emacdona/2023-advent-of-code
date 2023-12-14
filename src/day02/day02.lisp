;;;; day02.lisp

(in-package #:day02)

(defvar *input-file* "../../resources/day02/input")

(defun parse (line)
  (labels ((trim (s) (ppcre:regex-replace-all "^\\s+|\\s+$" s "")))
    (destructuring-bind
        (game-string rounds-string)
        (mapcar #'trim (ppcre:split ":" line :limit 2))
      (let* ((game (ppcre:regex-replace-all "^Game +" game-string ""))
             (rounds (mapcar #'trim (ppcre:split ";" rounds-string)))
             (moves
               (mapcar (lambda (m)
                         (mapcar #'trim (ppcre:split "," m)))
                       rounds))
             (moves-assoc
               (mapcar (lambda (ms)
                         (mapcar (lambda (m)
                                   (destructuring-bind
                                     (number color)
                                       (mapcar #'trim (ppcre:split " +" m :limit 2))
                                     `(,color . ,(parse-integer number))))
                                 ms))
                       moves)))
        (list (parse-integer game) moves-assoc)))))

;; This will let you see what the function above does
(defun get-games()
  (transform-lines *input-file* #'parse))

(defun possible-games (all-games &key num-red num-green num-blue)
  (remove-if
   (lambda (game)
     (some
      (lambda (moves-assoc)
        (or
         (> (or (cdr (assoc "red" moves-assoc :test #'string=)) 0) num-red)
         (> (or (cdr (assoc "green" moves-assoc :test #'string=)) 0) num-green)
         (> (or (cdr (assoc "blue" moves-assoc :test #'string=)) 0) num-blue)))
      (cadr game)))
   all-games))

(defun day-02-answer-01 ()
  (apply #'+
         (mapcar #'car
                 (possible-games
                  (get-games)
                  :num-red 12
                  :num-green 13
                  :num-blue 14))))

(defun power (game)
  (let ((rounds (cadr game)))
    (*
     (apply #'max (mapcar (lambda (moves-assoc)
                            (or
                             (cdr (assoc "red" moves-assoc :test #'string=))
                             1))
                          rounds))
     (apply #'max (mapcar (lambda (moves-assoc)
                            (or
                             (cdr (assoc "green" moves-assoc :test #'string=))
                             1))
                          rounds))
     (apply #'max (mapcar (lambda (moves-assoc)
                            (or
                             (cdr (assoc "blue" moves-assoc :test #'string=))
                             1))
                          rounds)))))

(defun day-02-answer-02 ()
  (apply
   #'+
   (mapcar #'power (get-games))))
