;;;; postrec.lisp

(in-package #:postrec)

;;;; database data models

(clsql:def-view-class repo ()
  ((id :type integer
       :db-constraints (:auto-increment :unique :not-null)
       :initarg :id)
   (name :type (string 256)
         :db-constraints (:not-null)
         :accessor name
         :initarg :name)
   (path :type (string 4096)
         :db-constraints (:not-null)
         :accessor path
         :initarg :path)))

;;;; database helpers

(defun with-db (fn &key db-path &allow-other-keys)
  "Get or create a db connection and pass it into fn."
  (clsql:with-database (db (list db-path)
                        :database-type :sqlite3)
    (apply fn (list db))))

(defun create-db-table (db vc)
  (clsql:create-view-from-class vc :database db))

(defun update-instance (db i)
  (clsql:update-records-from-instance i :database db))

(defun select-all (db)
  (clsql:select 'repo :database db :flatp t))

(defun create-repos-table (db)
  (with-db
    (lambda (d)
      (create-db-table d 'repo))
    :db-path db))

(defun repos (db)
  (with-db
    (lambda (d)
      (select-all d))
    :db-path db))

(defun update (db i)
  (with-db
    (lambda (d)
      (update-instance d i))
    :db-path db))

;;;; command line interface

(defun repos-get/options ())

(defun repos-get/handler (cmd)
  (loop for repo in (repos)
        do (format t "~A~%" (name repo))))

(defun repos-get/command ()
  (clingon:make-command
   :name "repos"
   :description "display a listing of repos"
   :options (repos-get/options)
   :handler #'repos-get/handler))

(defun get/options ())

(defun get/handler (cmd)
  (clingon:print-usage-and-exit cmd *STANDARD-OUTPUT*))

(defun get/command ()
  (clingon:make-command
   :name "get"
   :description "display a listing for a resource"
   :options (get/options)
   :handler #'get/handler
   :sub-commands (list (repos-get/command))))

(defun cli/options ()
  (list
   (clingon:make-option
    :string
    :description "user to greet"
    :short-name #\u
    :long-name "user"
    :initial-value "stranger"
    :env-vars '("CLI_USER")
    :key :user)))

(defun cli/handler (cmd)
  (clingon:print-usage-and-exit cmd *STANDARD-OUTPUT*))

(defun cli/command ()
  (clingon:make-command
   :name "postrec"
   :description "my first clingon cli app"
   :version "0.1.0"
   :license "BSD 2-Clause"
   :authors '("Tim Gallant <me@timgallant.us>")
   :options (cli/options)
   :handler #'cli/handler
   :sub-commands (list (get/command))))

(defun run-cli (argv)
  (clingon:run (cli/command) argv))
