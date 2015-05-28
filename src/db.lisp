;;; Copyright (C) 2015- Eike Kettner
;;
;;  This file is part of dlm.
;;
;; dlm is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; dlm is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

(in-package :dlm)

(defparameter *database* nil
  "The database file. Default is nil and can be set by the user.")

(defun %database ()
  (ensure-directories-exist
   (or *database*
       (merge-pathnames #p".config/dlm/dlm.db"
                        (user-homedir-pathname)))))

(defvar *metadata-keys*
  '(:SOURCE :KEEP :REDOWNLOADS :TIME :LIFETIME :LASTMOD :LENGTH :LOCATION
 :SHA256))

(defvar *pending-keys*
  '(:source :target :keep :lifetime :redownloads :time))

(defun %db-table-exists? (db table)
  "Check whether TABLE exists in DB."
  (let ((table-exists "select 1 from sqlite_master where type = ? and name = ?"))
    (execute-single db table-exists "table" table)))

(defun %db-init (db)
  (unless (%db-table-exists? db "dlm_files")
    (format t "Initializing database at ~a~%" (%database))
    (execute-non-query
     db "create table if not exists dlm_files
         (source text primary key,
          location text unique not null,
          lastmod integer,
          length integer,
          sha256 text,
          keep integer default 0,
          redownloads integer default 0,
          lifetime integer,
          time integer,
          createts integer default current_timestamp)"))
  (unless (%db-table-exists? db "dlm_pending")
    (execute-non-query
     db "create table if not exists dlm_pending
         (source text primary key,
          target text not null,
          keep integer default 0,
          lifetime integer,
          redownloads integer,
          time integer,
          createts integer default current_timestamp)")))

(defmacro with-database (var &body body)
  (if (listp var)
      (progn
        (unless (= (length var) 2)
          (error "Invalid binding list. Only two elements expected."))
        `(cond ((eq (type-of ,(cadr var)) 'sqlite-handle)
                (let ((,(car var) ,(cadr var)))
                  ,@body))
               ((not ,(cadr var))
                (with-open-database (,(car var) (%database))
                  (%db-init ,(car var))
                  ,@body))
               (t (error "Invalid form."))))
      `(with-open-database (,var (%database))
         (%db-init ,var)
         ,@body)))

(defparameter *db-converters*
  (flet ((tobool (x) (if (numberp x)
                         x (if x 1 0)))
         (pathstr (loc)
           (if (pathnamep loc)
                     (namestring loc)
                     loc)))
    (list
     :location #'pathstr
     :target #'pathstr
     :keep #'tobool
     :redownloads #'tobool))
  "A set of functions to convert to sql compatible data.")

(defun %db-sql-value (metadata key)
  "Convert the metadata value to something SQLish."
  (let ((convf (or (getf *db-converters* key) #'identity)))
    (funcall convf (getf metadata key))))

(defun db-metadata-exists? (metadata db)
  "Return t if METADATA exists in the database. That is, either
location or remote url can be found."
  (let ((url (%db-sql-value metadata :source))
        (loc (%db-sql-value metadata :location))
        (stmt "select count(*) from dlm_files where source = ? or location = ?"))
    (> (execute-single db stmt url loc) 0)))

(defun db-metadata-delete! (metadata db &key (table "dlm_files"))
  "Delete METADATA from database DB. Table is either 'dlm_files' or
'dlm_pending'."
  (let ((url (%db-sql-value metadata :source))
        (loc (%db-sql-value metadata :location))
        (stmt (format nil "delete from ~a where source = ? ~:[~;or location = ?~]"
                      table (string= table "dlm_files"))))
    (if (string= table "dlm_files")
        (execute-single db stmt url loc)
        (execute-single db stmt url))))


(defun %db-metadata->sql (metadata)
  "Creates the sql statement with placeholders and a list of its
values."
  (let ((keyvals (reduce
                  (lambda (tuple key)
                    (list (cons key (car tuple))
                          (cons (%db-sql-value metadata key) (cadr tuple))))
                  (remove-if-not #'keywordp metadata)
                  :initial-value '())))
    (cons (format nil "(~{~a~^,~}) values (~{~a~^,~})"
                  (car keyvals)
                  (make-list (length (car keyvals)) :initial-element "?"))
          (cadr keyvals))))

(defun %db-update (metadata db &key table)
  "Add METADATA to DB, replacing existing entries. The table argument
is either 'dlm_files' or 'dlm_pending'."
  (let* ((sqlvals (%db-metadata->sql metadata))
         (stmt (format nil "INSERT INTO ~a ~a"
                       table
                       (car sqlvals))))
    (db-metadata-delete! metadata db :table table)
    (apply #'sqlite:execute-non-query db stmt (rest sqlvals))))

(defun %db-make-metadata (values keys)
  "Creates the metadata plist from the given VALUES list by zipping it
with `keys'."
  (when-let (meta (mapcan #'list keys values))
    (cond-> meta
      ((equal 0 (getf meta :keep)) (update-metadata :keep nil))
      ((/= 0 (getf meta :keep)) (update-metadata :keep t)))))

(defun db-metadata-find (url db &key table (keycol "source"))
  "Finds a metadata entry for a given url."
  (let* ((keys (if (string= table "dlm_files") *metadata-keys* *pending-keys*))
         (cols (format nil "~{~a~^,~}" keys))
         (vals (execute-to-list
                db (format nil "select ~a from ~a where ~a = ?" cols table keycol) url)))
    (%db-make-metadata (first vals) keys)))

(defun db-metadata-find-file (filename db)
  (db-metadata-find filename db :table "dlm_files" :keycol "location"))

(defun db-upsert! (metadata db &key table)
  "Add METADATA to the database, replacing existing entries."
  (unless table
    (error "No table specified."))
  (unless (getf metadata :error)
    (%db-update metadata db :table table))
  metadata)

(defun db-search (f db &key where bindings (table "dlm_files"))
  "Query the database using WHERE statement with its BINDINGS and
apply F to each result. F is expected to take two arguments, the first
is the metadata plist and the second the current db handle."
  (let* ((keys (if (string= table "dlm_files") *metadata-keys* *pending-keys*))
         (cols (format nil "~{~a~^,~}" keys))
         (stmt (concat "select " cols " from " table
                       (if where " where " " ")
                       (or where "")
                       " order by time desc")))
    (loop
       with statement = (prepare-statement db stmt)
       initially (dolist (b bindings)
                   (bind-parameter statement (car b) (cdr b)))
       while (step-statement statement)
       do (let ((vals (loop
                         for i from 0 to (length *metadata-keys*)
                         collect (statement-column-value statement i))))
            (funcall f (%db-make-metadata vals keys) db))
       finally (finalize-statement statement))))

(defun db-collect (db &key where bindings (table "dlm_files"))
  "Query the database using WHERE statement with BINDINGS and collect
the resulting metadata into a list."
  (let ((result))
    (db-search #'(lambda (md db) (push md result))
               db :where where :bindings bindings :table table)
    (nreverse result)))

(defun db-collect-garbage (f &key db)
  "Go through all entries whose lifetime has expired. Check if the
file exists locally. If it does, apply F to it. Otherwise update db
entry."
  (flet ((checkfn (md db)
           (when (probe-file (getf md :location))
             (funcall f md db))))
    (with-database (curdb db)
      (db-search #'checkfn curdb
                 :where (format nil "time + lifetime < ~a and keep = 0"
                                (get-universal-time))))))

(defmacro db-with-pending ((var (url target keep lifetime redownloads) db) &body body)
  "Create a db entry using the given values or load an existing one
from the database. The var is either bind to the existing one or the
the given values converted into a plist. Then body evaluates and if it
returns normally the record is deleted."
  (let ((meta (gensym))
        (curdb (gensym))
        (found (gensym)))
    `(let ((,meta (list :source ,url
                        :target (or ,target (merge-pathnames ""))
                        :keep ,keep
                        :lifetime ,lifetime
                        :redownloads ,redownloads))
           (,found (with-database (,curdb ,db)
                     (db-metadata-find ,url ,curdb :table "dlm_pending"))))
       (unless ,found
         (with-database (,curdb ,db)
           (db-upsert! (append (list :time (get-universal-time)) ,meta) ,curdb :table "dlm_pending")))
       (prog1
           (let ((,var (or ,found ,meta)))
             ,@body)
         (with-database (,curdb db)
           (db-metadata-delete! ,meta ,curdb :table "dlm_pending"))))))
