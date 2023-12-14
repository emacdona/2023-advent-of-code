;;;; advent-of-code-2023.asd

(asdf:defsystem #:advent-of-code-2023
  :description "My attempted solutions for Advent of Code 2023."
  :author "Ed MacDonald"
  :license  "I'm not sure yet."
  :version "0.0.1"
  :serial t
  :depends-on ("cl-ppcre" "alexandria" "fiveam")
  :components ((:file "package")
               (:file "common" :pathname "src/common/common")
               (:file "day01" :pathname "src/day01/day01")
               (:file "day02" :pathname "src/day02/day02")
               (:file "day03" :pathname "src/day03/day03")
               (:file "tests" :pathname "test/tests")
               ))
