;;;; bomber-chap.asd

(asdf:defsystem #:bomber-chap
  :description "Gamejam times!"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:daft)
  :components ((:file "package")
               (:file "base")
               (:file "level")
               (:file "main")))
