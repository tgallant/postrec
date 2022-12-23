;;;; postrec.asd

(asdf:defsystem #:postrec
  :description "Describe postrec here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:clingon #:clsql)
  :components ((:file "package")
               (:file "postrec")))
