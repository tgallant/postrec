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

(defun create-db-table (db vc)
  (clsql:create-view-from-class vc :database db))

(defun ensure-db-setup (db)
  (if (not (clsql:table-exists-p "REPO" :database db))
      (create-db-table db 'repo)))

(defmacro with-db (db-path &body body)
  `(clsql:with-database (db (list ,db-path) :database-type :sqlite3)
     (ensure-db-setup db)
     ,@body))

(defun update-instance (db i)
  (clsql:update-records-from-instance i :database db))

(defun select-all (db)
  (clsql:select 'repo :database db :flatp t))

(defun create-repos-table (db-path)
  (with-db db-path
    (create-db-table db 'repo)))

(defun repos (db-path)
  (with-db db-path
    (select-all db)))

(defun update (db-path i)
  (with-db db-path
    (update-instance db i)
    i))

;;;; command line interface

(defun format-repo (r)
  (format t "~A ~A~%" (name r) (path r)))

(defun repo-create/options ()
  (list
   (clingon:make-option
    :string
    :description "the name of the repo."
    :short-name #\n
    :long-name "name"
    :initial-value "postrec.db"
    :env-vars '("POSTREC_DB")
    :key :name)
   (clingon:make-option
    :string
    :description "the path for the repo."
    :short-name #\p
    :long-name "path"
    :initial-value "postrec.db"
    :env-vars '("POSTREC_DB")
    :key :path)))

(defun repo-create/handler (cmd)
  (let ((db (clingon:getopt cmd :db))
        (name (clingon:getopt cmd :name))
        (path (clingon:getopt cmd :path)))
    (format-repo (update db (make-instance 'repo :name name :path path)))))

(defun repos-create/command ()
  (clingon:make-command
   :name "repo"
   :description "create a repo"
   :options (repo-create/options)
   :handler #'repo-create/handler))

(defun create/options ())

(defun create/handler (cmd)
  (clingon:print-usage-and-exit cmd *STANDARD-OUTPUT*))

(defun create/command ()
  (clingon:make-command
   :name "create"
   :description "create a resource"
   :options (create/options)
   :handler #'create/handler
   :sub-commands (list (repos-create/command))))

(defun repos-get/options ())

(defun repos-get/handler (cmd)
  (let ((db (clingon:getopt cmd :db)))
    (loop for repo in (repos db)
          do (format t "~A~%" (name repo)))))

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
    :description "the database to use."
    :short-name #\d
    :long-name "database"
    :initial-value "postrec.db"
    :env-vars '("POSTREC_DB")
    :key :db)))

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
   :sub-commands (list (create/command)
                       (get/command))))

(defun cli/run (argv)
  (clingon:run (cli/command) argv))
