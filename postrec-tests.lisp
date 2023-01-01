;;;; postrec-tests.lisp

(in-package :cl-user)
(defpackage postrec-tests
  (:use :cl :postrec :fiveam :cl-mock))

(in-package :postrec-tests)

(def-suite db-tests
  :description "Tests for postrec!")

(in-suite db-tests)

(defmacro with-test-env (&body body)
  `(uiop:call-with-temporary-file
    (lambda (db)
        (with-mocks ()
          (answer (clingon.utils::exit 0) nil)
          ,@body))
    :want-pathname-p t
    :want-stream-p nil
    :type "db"))

(defmacro capture-output (&body body)
 `(with-output-to-string (*STANDARD-OUTPUT*)
    ,@body))

(defun test-cli (cmd test-fn)
  (is-every))

(test create-repo
  (let ((output (with-test-env
                  (postrec::cli/run `( "-d" ,db "create" "repo" "-n" "t1" "-p" "/tmp/t1"))
                  (is (= 1 (length (postrec::repos db)))))))
    (is-true (search "t1 /tmp/t1" output))))

(run! 'db-tests)
