;;;; postrec-tests.lisp

(in-package :cl-user)
(defpackage postrec-tests
  (:use :cl :postrec :fiveam))

(in-package :postrec-tests)

(def-suite db-tests
  :description "Tests for postrec!")

(in-suite db-tests)

(defun with-temp-db (fn)
  (uiop:call-with-temporary-file
   fn :want-pathname-p t :want-stream-p nil :type "db"))

(defun make-mock-repo ()
  (make-instance 'postrec::repo
                 :name "test repo"
                 :path "/var/postrec/test_repo"))

(test create-repos-table
  (with-temp-db
      (lambda (db)
        (postrec::create-repos-table db)
        (postrec::update db (make-mock-repo))
        (is (= 1 (length (postrec::repos db)))))))

;; (run! 'db-tests)
