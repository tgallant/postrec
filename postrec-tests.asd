;;;; postrec-tests.asd

(asdf:defsystem #:postrec-tests
  :depends-on (#:postrec #:fiveam)
  :components ((:file "postrec-tests")))
