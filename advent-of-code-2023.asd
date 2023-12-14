;;;; advent-of-code-2023.asd

(asdf:defsystem #:advent-of-code-2023
  :description "Describe advent-of-code-2023 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
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
