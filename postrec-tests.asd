;;;; postrec-tests.asd

(asdf:defsystem #:postrec-tests
  :depends-on (#:postrec #:fiveam #:cl-mock)
  :components ((:file "postrec-tests")))
