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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :ironclad)
    (require 'ironclad))

  (unless (find-package :external-program)
    (require 'external-program)))

(defparameter *file-lifetime*
  (* 30 24 60 60)
  "The default lifetime of a file in seconds.")

(defparameter *fetch-default-bin* "curl"
  "The executable that is used by default to download.")

(defparameter *fetch-default-args* "-O#C - ~a"
  "Options to use for downloading files. The string is first expanded
  with the url using `format'. There must be one ~a control string
  specified that is replaced witht he url.")

(defparameter *default-target* nil
  "Default target directory used to put downloaded files. This is nil
  by default, meaning that each file is put into the current
  directory.")

(defparameter *which-program* "which"
  "Used to check for existence of other programs used here. Only
  needed, if programs are not specified with full path.")

(defparameter *fetch-configs*
  nil
  "An list of fetch-config objects that consists of a predicate
  function that accepts an url and returns T if the fetch function of
  the fetch-config object should be used to download the file at the
  url. This function must then download the file to the current
  directory and return the filename. If this list is nil, or no
  download function can be determined, a fallback one via
  `*fetch-default-bin*' is used.")

(defparameter *youtube-dl-bin* "youtube-dl"
  "The youtube-dl executable.")

(defparameter *youtube-dl-args* "--prefer-free-formats --no-playlist ~a"
  "Options for youtube-dl to fetch video urls.")

(defparameter *download-notify-hook* nil)

(defun make-file-metadata (filename)
  "Return few meta data about a file. Throw if file does not exist."
  (let ((file (probe-file filename)))
    (unless file
      (error (format nil "File does not exist: ~a" filename)))
    (list
     :lastmod (file-write-date file)
     :length (with-open-file (in file :element-type '(unsigned-byte 8))
               (file-length in))
     :time (file-atime file)
     :location (namestring file)
     :sha256 (ironclad:byte-array-to-hex-string
              (ironclad:digest-file :sha256 file)))))

(defun make-download-metadata (filename &key source keep (lifetime *file-lifetime*) (redownloads 0))
  (append (list
           :source source
           :keep keep
           :redownloads redownloads
           :lifetime lifetime)
          (make-file-metadata filename)))

(defun check-file-metadata (metadata)
  "Check METADATA plist against its file. Return a list of plists each
containing the :name of the key, the :value as provided in METADATA
and the different :expected value. If no mismatches are found, return
nil."
  (let* ((filename (getf metadata :location))
         (file (probe-file filename))
         (expected (if file (make-file-metadata file)))
         (keys (delete :time (remove-if-not #'keywordp expected))))
    (if file
        (delete nil
                (mapcar (lambda (key)
                          (let ((value (getf metadata key))
                                (expval (getf expected key)))
                            (if (equalp value expval) nil
                                (list :name key
                                      :value value
                                      :expected expval))))
                        keys))
        'file-not-found)))

(defun file-metadata-valid? (metadata)
  (not (check-file-metadata metadata)))


(defun update-metadata (metadata &key (keep nil keep-p)
                                   (lifetime nil lifetimep)
                                   (location nil locationp)
                                   (source nil sourcep)
                                   (redownloads nil redownloadsp))
  "Replace the provided data in the given metadata. Return a new
metadata plist."
  (cond-> metadata
    (keep-p (plist-replace :keep keep))
    (lifetimep (plist-replace :lifetime lifetime))
    ((and locationp location) (plist-replace :location location))
    ((and sourcep source) (plist-replace :source source))
    (redownloadsp (plist-replace :redownloads redownloads))))

(defun program-exists? (name)
  (or (probe-file name)
      (multiple-value-bind (rckey rcval)
          (external-program:run *which-program* (list name))
        (declare (ignore rckey))
        (= 0 rcval))))

(defun fetch-default (url &optional user pass)
  (declare (ignore user) (ignore pass))
  (let ((opts (string-split #\Space (format nil *fetch-default-args* url))))
    (multiple-value-bind (rckey rcval)
        (external-program:run *fetch-default-bin* opts :input t :output t)
      (declare (ignore rckey))
      (when (= rcval 0)
        (file-namestring url)))))

(defun fetch-yt-get-filename (url)
  (let ((args (remove "~a" (string-split #\Space *youtube-dl-args*) :test #'string=)))
    (when (program-exists? *youtube-dl-bin*)
      (let ((nameout (with-output-to-string (s)
                       (external-program:run *youtube-dl-bin*
                                             (append args
                                                     `("-s" "--get-filename" ,url))
                                             :output s))))
        (when (and nameout
                   (> (length nameout) 0))
          (subseq nameout 0 (1- (length nameout))))))))

(defun fetch-yt-url? (url)
  (when (program-exists? *youtube-dl-bin*)
    (let* ((url-list (string-split '(#\Space #\Newline)
                                   (with-output-to-string (s)
                                     (external-program:run *youtube-dl-bin*
                                                           `("--list-extractors")
                                                           :output s))))
           (urls (mapcar #'string-downcase url-list))
           (urldc (string-split '(#\. #\/) (string-downcase url))))
      (find-if (lambda (extr)
                 (find extr urldc :test #'string=))
               urls))))

(defun fetch-yt (url &optional user pass)
  (declare (ignore user) (ignore pass))
  (let ((args (string-split #\Space (format nil *youtube-dl-args* url)))
        (filename (fetch-yt-get-filename url)))
    (multiple-value-bind (rckey rcval)
        (external-program:run *youtube-dl-bin* args :input t :output t)
      (declare (ignore rckey))
      (when (= rcval 0)
        filename))))


(defclass fetch-config ()
  ((can-fetch? :initarg :can-fetch?
               :initform (error "All slots for fetch-config must be specified.")
               :documentation "A function taking an url and return
               true, if this config should be used to download the
               file.")
   (fetch :initarg :fetch
          :initform (error "All slots for fetch-config must be specified.")
          :documentation "A function taking an url and optionally a
          user and pass argument that downloads the file at the given
          url. It stores the file in the current directory.")))

(defmethod initialize-instance :after ((fc fetch-config) &key)
  (with-slots (can-fetch? fetch) fc
    (unless (functionp can-fetch?)
      (error "can-fetch? must be a function!"))
    (unless (functionp fetch)
      (error "fetch must be a function!"))))

(defvar *default-fetch-config*
  (make-instance 'fetch-config
                 :can-fetch? #'identity
                 :fetch #'fetch-default))

(defvar *youtube-dl-fetch-config*
  (make-instance 'fetch-config
                 :can-fetch? #'fetch-yt-url?
                 :fetch #'fetch-yt))

(defun add-fetch-config (&key can-fetch? fetch)
  "Add a new fetch-config object at the beginning of the list."
  (push (make-instance 'fetch-config
                       :can-fetch? can-fetch?
                       :fetch fetch)
        *fetch-configs*))


(defun add-fetch-config-default ()
  (setq *fetch-configs*
        (adjoin *default-fetch-config* *fetch-configs*)))

(defun add-fetch-config-youtube ()
  (setq *fetch-configs*
        (adjoin *youtube-dl-fetch-config* *fetch-configs*)))

(setq *fetch-configs*
      (list *youtube-dl-fetch-config*
            *default-fetch-config*))

(defun fetch-get-download-fn (url)
  (flet ((can-fetch (fc)
           (funcall (slot-value fc 'can-fetch?) url)))
    (let ((fc (find-if #'can-fetch *fetch-configs*)))
      fc)))

(defun fetch-file (url target &key user pass)
  "Fetch the single file at URL and store it in TARGET directory. Use
current directory if TARGET is nil. Return a plist from
`make-download-metadata' without setting `keep', `lifetime' and
`redownload'."
  (with-current-dir (or target *default-target*)
    (let* ((fconfig (fetch-get-download-fn url))
           (result (apply (slot-value fconfig 'fetch) url (list user pass))))
      (if result
          (make-download-metadata result :source url)
          (list :error "Error downloading file.")))))


(defun download (url target &key keep lifetime (redownloads 0) user pass db)
  "Download the single file at URL and store it at TARGET
directory. If TARGET is nil, use current directory. Save file metadata
and the given metadata into the database."
  (db-with-pending (_pending (url (or target *default-target*) keep
                                  (or lifetime *file-lifetime*) redownloads) db)
    (declare (ignore _pending))
    (if (> redownloads 0)
        (format t "Redownload ~a → ~a ~%" url (or target *default-target* "."))
        (format t "Download ~a → ~a ~%" url (or target *default-target* ".")))
    (flet ((upsert (metadata)
             (with-database (curdb db)
               (db-upsert! metadata curdb :table "dlm_files"))))
      (let ((result
             (-> (fetch-file url target :user user :pass pass)
               (update-metadata :redownloads redownloads
                                :keep keep
                                :lifetime (or lifetime *file-lifetime*))
               (upsert))))
        (run-hooks *download-notify-hook* result nil)
        result))))

(defun dlm-local-source? (metadata)
  "Return true if metadata has a local source instead of a remote
url."
  (string= (getf metadata :source)
           (getf metadata :location)))

(defun dlm-redownload (metadata &key user pass db)
  "Redownload a file. Same as `download' but increments the
`redownloads' counter. Overwrites existing files."
  (let* ((downloads (getf metadata :redownloads))
         (filename (getf metadata :location))
         (target (directory-namestring filename))
         (url (getf metadata :source)))
    (when (dlm-local-source? metadata)
      (error "~a: cannot download from a local source." filename))
    (download url target
              :db db
              :user user
              :pass pass
              :keep (getf metadata :keep)
              :redownloads (1+ downloads)
              :lifetime (getf metadata :lifetime))))

(defun dlm-delete-file (metadata &key silent dry test db)
  "Deletes the file identified by METADATA from the filesystem and
updates the database.

For each file that is deleted a short message is printed to stdout,
unless `silent' is truthy. If `dry' is true, the file is not
deleted. With the `test' argument a predicate function can be
specified that must accept two arguments: the metadata plist and the
file (a pathname). Only if it returns truthy, the file is deleted. "
  (let* ((filename (getf metadata :location))
         (file (probe-file filename)))
    (cond
      ((getf metadata :keep)
       (unless silent
         (format t "Keep ~a~%" filename))
       nil)
      ((and file (or (not test) (funcall test metadata file)))
       (unless silent
           (format t "Deleting ~a ...~%" file))
       (unless dry
         (delete-file file)
         (when (dlm-local-source? metadata)
           (with-database (curdb db)
             (db-metadata-delete! metadata curdb :table "dlm_files"))))
       metadata))))

(defun dlm-download (url &key target keep lifetime user pass db)
  "Downloads the file at URL into the TARGET directory. A database
entry is created to hold the additional meta information. LIFETIME is
a time in seconds that must pass after last access to that file in
order to delete it when `dlm-collect-garbage' is run."
  (let ((existing (with-database (curdb  db)
                    (db-metadata-find url curdb :table "dlm_files"))))
    (if existing
        (if (file-metadata-valid? existing)
            (progn
              (format t "File already exists.~%")
              (run-hooks *download-notify-hook* existing t)
              existing)
            (dlm-redownload existing :user user :pass pass :db db))
        (download url target
                  :keep keep
                  :lifetime lifetime
                  :redownloads 0
                  :user user
                  :pass pass
                  :db db))))

(defun dlm-collect-garbage (&key silent dry db)
  "Deletes files whose lifetime has expired. Print info messages to
stdout about each deleted file unless `silent' is true. If `dry' is
true, do not delete anything."
  (let ((counter 0))
    (flet ((pred (md file)
             (let* ((secs (file-atime-since file))
                    (lifetime (getf md :lifetime)))
               (when (> secs lifetime)
                 (incf counter) t))))
      (db-collect-garbage
       #'(lambda (md db)
           (dlm-delete-file md
                            :db db
                            :test #'pred
                            :silent silent
                            :dry dry))
       :db db))
    counter))


(defun %make-sql-expression (metadata key)
  (when (getf metadata key)
    (if (member key '(:redownloads :keep))
        (format nil "~a = ~a" key (%db-sql-value metadata key))
        (format nil "~a like '~a'" key (%db-sql-value metadata key)))))

(defun dlm-format-metadata (metadata)
  "Make a one-line string from METADATA."
  (let* ((filename (getf metadata :location))
         (source (getf metadata :source))
         (keep (getf metadata :keep))
         (file (if filename (probe-file filename)))
         (ltime (or (getf metadata :lifetime) 0))
         (secs (or (and file (file-atime-since file)) 0)))
    (format nil "[~:[.~;k~]~:[!~;e~]][~3d] ~a ~a ~a [~a]"
            keep
            (and file (probe-file file))
            (getf metadata :redownloads)
            (if (and (not keep) file)
                (let ((until (- ltime secs)))
                  (if (minusp until)
                      "exprd."
                      (format-duration until)))
                "  --  ")
            (format-bytes (getf metadata :length))
            filename
            (if (string= source filename) "-" source))))

(defun dlm-print-metadata (metadata &optional db)
  "Prints the given metadata to stdout in one line. The second
parameter is not used, but declared anyways to make it compatible to
`dlm-query'."
  (declare (ignore db))
  (format t "~a~%" (dlm-format-metadata metadata)))

(defun dlm-query (f meta-template &key filter (table "dlm_files"))
  "Queries the database according to the search template
META-TEMPLATE. This is a plist just like normal metadata, but may not
specify all properties and values may contain the wildcard character
'%'. F is a function accepting two arguments that is applied to each
metadata found which is not excluded by the optional FILTER
function. The second argument is the current db handle."
  (let* ((clauses (mapcar (curry #'%make-sql-expression meta-template)
                          (remove-if-not #'keywordp meta-template)))
         (query (format nil "~{~a ~^and ~}" (delete-if #'not clauses)))
         (fun (or f #'dlm-print-metadata))
         (pred (or filter #'identity)))
    (with-database db
      (db-search (lambda (md db)
                   (when (funcall pred md)
                     (funcall fun md db)))
                 db :where (non-empty query) :table table))))

(defun dlm-change-metadata (metadata &key (keep nil keep-p) (lifetime nil lifetimep) db)
  "Changes the keep flag and/or the lifetime value of the given
metadata and updates the database."
  (let ((newmeta  (cond-> metadata
                    (keep-p (plist-replace :keep keep))
                    (lifetimep (plist-replace :lifetime lifetime)))))
    (with-database (curdb db)
      (db-upsert! newmeta curdb :table "dlm_files"))))

(defun dlm-move (metadata target &key db silent)
  "Move the file specified by METADATA to the new TARGET directory,
updating the database accordingly."
  (let* ((filename (getf metadata :location))
         (file (probe-file filename)))
    (unless (or file silent)
      (format t "Skip '~a' as it does not exist.~%" filename))
    (when file
      (ensure-directories-exist target)
      (let* ((newfile (merge-pathnames (file-namestring file)
                                       (truename target)))
             (newmeta (if newfile
                          (update-metadata metadata
                                           :location (namestring newfile)))))
        (unless (equalp file newfile)
          (unless silent
            (format t "Move ~a → ~a~%" file newfile))
          (rename-file file newfile)
          (db-metadata-delete! metadata db :table "dlm_files")
          (if (dlm-local-source? metadata)
              (db-upsert! (update-metadata newmeta :source (namestring newfile)) db
                          :table "dlm_files")
              (db-upsert! newmeta db :table "dlm_files")))))))

(defun dlm-prune (metadata &key silent db)
  "Delete the file specified with METADATA and removes the record from
  the database. After that dlm has completely forgotten about this
  file."
  (let* ((filename (getf metadata :location))
         (file (probe-file filename)))
    (cond
      ((getf metadata :keep)
       (unless silent
         (format t "Keep ~a~%" filename))
       nil)
      (t
       (when file
         (delete-file file))
       (with-database (curdb db)
         (db-metadata-delete! metadata curdb :table "dlm_files"))
       (unless silent
         (format t "Prune file ~a~%" filename))
       (unless (or file silent)
         (format t "Skip deleting '~a' as it does not exist.~%" filename))))))

(defun dlm-add-local (filename &key keep lifetime db)
  (let* ((file (probe-file filename))
         (meta (make-download-metadata file
                                       :source filename
                                       :keep keep
                                       :lifetime (or lifetime *file-lifetime*))))
    (with-database (curdb db)
      (db-upsert! meta curdb)
      meta)))

(defun dlm-fetch (file-or-url &key target keep lifetime user pass)
  (if (handler-case
          (probe-file file-or-url)
        (error () nil))
      (dlm-add-local file-or-url :keep keep :lifetime lifetime)
      (dlm-download file-or-url :target target :keep keep :lifetime lifetime :user user :pass pass)))

(defun %dlm-clear (url table &key silent db)
  (with-database (curdb db)
    (let ((meta (db-metadata-find url curdb :table table)))
      (if meta
          (progn
            (unless silent
              (format t "Clearing ~a~%" (getf meta :source)))
            (db-metadata-delete! meta curdb :table table))
          (unless silent
            (format t "No entry found for ~a~%" url))))))

(defun dlm-clear-pending (url &key silent db)
  (%dlm-clear url "dlm_pending" :silent silent :db db))

(defun dlm-clear-metadata (url &key silent db)
  (%dlm-clear url "dlm_files" :silent silent :db db))

(defun dlm-info (filename &key db)
  "Return all info about given FILENAME. If FILENAME is not found
return a plist with an :error keyword and value of either
'file-not-found or 'file-unknown. Otherwise the plist
contains :file-metadata, :db-metadata, :checks and :metadata-valid?."
  (let* ((file (probe-file filename))
         (metadb (if file
                     (with-database (curdb db)
                       (db-metadata-find-file (namestring file) curdb))))
         (metareal (make-file-metadata filename))
         (checks (if metadb (check-file-metadata metadb))))
    (cond ((not file)
           '(:error :file-not-found :msg "The file does not exist."))
          ((not metadb)
           '(:error :file-unknwon :msg "The file is not known to dlm."))
          (t
           (list :metadata-valid? (not checks)
                 :checks checks
                 :file-metadata metareal
                 :db-metadata metadb)))))
