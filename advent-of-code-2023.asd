;;;; advent-of-code-2023.asd

(asdf:defsystem #:advent-of-code-2023
  :description "Describe advent-of-code-2023 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-ppcre" "alexandria")
  :components ((:file "package")
               (:file "advent-of-code-2023-01" :pathname "src/day01/day01")))
