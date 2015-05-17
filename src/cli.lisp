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

(in-package :dlm-cli)

(defvar *default-command* "fetch")

(set-options
  `( ;; bool-options
  verbose ((#\v nil) nil "Be more verbose.")
  keep    ((#\k "keep") nil "Flag the file to never be deleted by dlm")
  silent  ((#\s "silent") nil "Don't print info messages.")
  dry     ((#\d "dry") nil "Don't actually perform the action.")
  qkeep    ((#\k "keep") nil "Search files that are specified to not be deleted.")
  existing ((#\e "existing") nil "Show only files that exists on disk.")
  nonexisting ((#\r "nonexisting") nil "Show files that do not exist on disk.")
  valid   ((#\c "valid") nil "Show files where its metadata (sha256, size, lastmod timestamp) matches the db entry.")
  invalid ((#\C "invalid") nil "Show files where its metadata (sha256, size, lastmod timestamp) does not match the db entry.")
  noheader ((#\H "no-header") nil "Don't print search parameters header.")
  raw-values ((#\r "raw-values") nil "Print raw db data values instead of human readable form.")

  ;; parameters
  limit   ((#\l "limit") "" "Apply the action to the first 'n' items only.")
  name    ((#\n "name") "" "Search in the filename.")
  source  ((#\s "source") "" "Search in the url.")
  lifetime ((#\l "lifetime") "" ,(format nil "The lifetime to set for this file. Default is ~a." (format-duration *file-lifetime*)))
  user    ((#\u "user") "" "The username to authenticate with.")
  pass    ((#\p "pass") "" "The password to authenticate with.")
  target  ((#\t "target") "./" "The target directory")))

(defcommand fetch (keep &parameters target lifetime user pass &free urls)
  "Download a file at some url.

This will call other download programs like curl in order to download
a file at the given url. If multiple urls are specified they are
downloaded sequentially. What download program to use is determined by
the url. If, for example, the youtube-dl tool is available, certain
urls to video portals are downloaded using this tool. Otherwise curl
is the default fallback (unless configured otherwise).

The '--user' and '--pass' options are simply delegated to the real
download programs. Thus it depends on whether the program supports
these options.

If URL is a path to a local file, it is simply added to the database
and the source and location properties are both set to the same
path. Those files don't have a remote source and are treated a little
different. First, 'deleteing' them causes the db entry to be removed,
too. Then, obviously, they cannot be redownloaded and will be skipped
if tried."
  (unless urls
    (echo "Nothing to fetch. See `help'."))
  (let ((time (if (empty? lifetime) nil  (parse-duration lifetime)))
        (failures?))
    (dolist (url urls)
      (let ((result
             (dlm-fetch url :target target
                        :keep keep
                        :user user
                        :pass pass
                        :lifetime time)))
        (when-let (err (getf result :error))
          (setq failures? t)
          (echo err))))
    (when failures?
      (user-error "Some files could not be downloaded."))))

(defcommand collect-garbage (silent dry)
  "Deletes expired files.

Checks the last access time of each file with expired lifetime in the
db and deletes it, if it is older than the specified lifetime for this
file."
  (unless silent
    (echo "Collecting garbage..."))
  (let ((num (dlm-collect-garbage :silent silent :dry dry)))
    (unless silent
      (echo "Deleted ~d files." num))))

(defmacro %search-params-str (&body list)
  (let ((s (gensym)))
    `(with-output-to-string (,s)
       ,@(mapcar (lambda (el)
                   `(if (listp ,el)
                        (when (cadr ,el)
                          (format ,s "~a:~(~a~) " (car ,el) (cadr ,el)))
                        (when ,el
                          (format ,s "~(~a~):~(~a~) " ',el ,el))))
                 list))))

(defun %query-action-fetch (md db)
  (if (file-metadata-valid? md)
      (echo "File already exists.")
      (let ((url (getf md :source))
            (file (getf md :location)))
        (if (string= url file)
            (echo "Cannot download from local source: ~a" file)
            (dlm-redownload md :db db)))))

(defun %query-action-delete (md db)
  (dlm-delete-file md :db db))

(defun %query-action-keep (md db)
  (unless (getf md :keep)
    (echo "Keep file ~a" (getf md :location))
    (dlm-change-metadata md :keep t :db db)))

(defun %query-action-nokeep (md db)
  (when (getf md :keep)
    (echo "Don't keep file ~a" (getf md :location))
    (dlm-change-metadata md :keep nil :db db)))

(defun %query-action-set-lifetime (secs md db)
  (let* ((time (handler-case
                   (parse-integer secs)
                 (error ()
                   (parse-duration secs))))
         (valid (and time (/= time 0))))
    (unless valid
      (echo "Invalid lifetime: ~a" secs))
    (when (or valid (= (getf md :lifetime) time))
      (echo "Setting lifetime of ~a to ~a" (getf md :location) (format-duration time))
      (dlm-change-metadata md :lifetime time :db db))))

(defun %query-action-move (newtarget md db)
  (dlm-move md newtarget :db db))

(defun %query-action-prune (md db)
  (dlm-prune md :db db))

(defun %query-action-clear (md db)
  (dlm-clear-metadata (getf md :source) :db db))

(defun %query-action-shortlist (md db)
  (declare (ignore db))
  (echo "~a" (getf md :location)))

(defun %query-filter-limit (max)
  (let ((counter 0))
    (lambda (md)
      (declare (ignore md))
      (if (< counter max)
          (incf counter)))))

(defun %query-filter-exists (md)
  (probe-file (getf md :location)))

(defun %query-filter-valid-metadata (md)
  (file-metadata-valid? md))

(defun get-action-by-prefix (prefix action-list)
  "Get the actions from ACTION-LIST whose name starts with PREFIX."
  (flet ((prefix? (action)
           (starts-with? prefix action)))
    (remove-if-not #'prefix? action-list :key #'car)))

(defun %action-candidates-string (prefix candidates)
  (with-output-to-string (s)
    (format s "The action name '~a' is ambiguous. What did you mean?~%" prefix)
    (format s "~{- ~a~%~}~%" (mapcar #'car candidates))))


(defun make-action-fn (fn)
  "Return a function that accepts multiple arguments and either
returns FN if those are nil or a curried function of FN and ARGS."
  (lambda (&rest args)
    (if (empty? args) fn
        (apply #'curry fn args))))

(defparameter *query-actions-alist*
  `(("list" . ,(make-action-fn #'dlm-print-metadata))
    ("short-list" . ,(make-action-fn #'%query-action-shortlist))
    ("move" . ,(make-action-fn #'%query-action-move))
    ("set-lifetime" . ,(make-action-fn #'%query-action-set-lifetime))
    ("prune" . ,(make-action-fn #'%query-action-prune))
    ("clear" . ,(make-action-fn #'%query-action-clear))
    ("keep" . ,(make-action-fn #'%query-action-keep))
    ("nokeep" . ,(make-action-fn #'%query-action-nokeep))
    ("fetch" . ,(make-action-fn #'%query-action-fetch))
    ("delete" . ,(make-action-fn #'%query-action-delete))))

(defparameter *query-actions-argn*
  '(("move" . 1)
    ("set-lifetime" . 1)))

(defun %find-query-action (action-args actions-alist argn-alist &optional default)
  (let* ((name (or (first action-args) default))
         (args (rest action-args))
         (candidates (get-action-by-prefix name actions-alist))
         (cmd (car candidates))
         (argn (cdr (assoc (car cmd) argn-alist :test #'equalp)))
         (action-fn
          (cond ((> (length candidates) 1)
                 (user-error (%action-candidates-string name candidates)))
                ((null cmd)
                 (user-error "Action '~a' not found." name))
                (t (or (apply (cdr cmd) args)
                       (error "Action function not defined for: ~a" name))))))
    (cond
      ((functionp argn)
       (unless (funcall argn args)
         (user-error "The arguments '~a' to action '~a' are invalid." args (car cmd))))
      ((numberp argn)
       (unless (= (length args) argn)
         (user-error "Invalid number of arguments (~d) to action \"~a\". Requires ~d."
                     (length args)
                     name
                     argn)))
      ((not argn)
       (unless (= (length args) 0)
         (user-error "The action '~a' does not take arguments." (car cmd))))
      (t (user-error "Invalid value in argn-list: ~s" argn)))
    action-fn))


(defcommand query (qkeep existing nonexisting valid invalid noheader &parameters name source limit &free action)
  "Query the database for files that have been downloaded.

Each search option that takes an argument is compared against the
corresponding field. You can use % character as a wildcard (matching
many characters).

Actions are then applied to each file in the result. Actions are

- list: (the default) print information to stdout
- short-list: print only the local filename to stdout
- fetch: download the file again, if the file exists and the metadata
  matches the db, it is not downloaded again
- delete: deletes the file on disk, but not the record in the database
- prune: deletes the file and removes the record from the database
- clear: deletes the record from the db but leaves the file on disk
- keep: set the keep flag to true
- nokeep: set the keep flag to false
- move [dir]: moves the file to the given directory
- set-lifetime [secs]: set a new lifetime in seconds or use \"2d10M\"
  strings (you can use m,d,h and M)

Action names can be abbreviated to the shortest non-ambiguous
name. Only one action can be given at a time. Note that checking file
metadata (the -C|c option) involves computing a checksum of the file
which may take some time depending on its size."
  (let* ((action-fn (%find-query-action action
                                        *query-actions-alist*
                                        *query-actions-argn*
                                        "list"))
         (filter (conjunct
                  (if existing #'%query-filter-exists #'identity)
                  (if nonexisting (lambda (md) (not (%query-filter-exists md))) #'identity)
                  (if valid #'%query-filter-valid-metadata #'identity)
                  (if invalid (lambda (md) (not (%query-filter-valid-metadata md))) #'identity)
                  (if (empty? limit) #'identity (%query-filter-limit (as-int limit))))))
    (unless noheader
      (let ((searchstr (non-empty
                        (%search-params-str `("keep" ,qkeep) name source existing limit))))
        (format t "Looking for ~@[~a~]~:[all~;~]~%" searchstr searchstr)))
    (dlm-query action-fn `(:location ,name :source ,source :keep ,qkeep)
                 :filter filter)))

(defun %pending-action-print (md db)
  (declare (ignore db))
  (format t "~a: ~a → ~a~%"
          (get-time-string (getf md :time))
          (getf md :source)
          (getf md :target)))

(defun %pending-action-fetch (md db)
  (let ((url (getf md :source))
        (target (getf md :target))
        (lifetime (or (getf md :lifetime) *file-lifetime*))
        (keep (or (getf md :keep) 0)))
    (dlm-download url :target target :keep keep :lifetime lifetime :db db)))

(defun %pending-action-clear (md db)
  (let ((url (getf md :source)))
    (dlm-clear-pending url :db db)))


(defparameter *pending-actions-alist*
  `(("list". ,(make-action-fn #'%pending-action-print))
    ("clear" . ,(make-action-fn #'%pending-action-clear))
    ("fetch" . ,(make-action-fn #'%pending-action-fetch))))

(defparameter *pending-actions-argn*
  nil)

(defcommand pending (qkeep noheader &parameters source limit &free action)
  "Show pending downloads.

This queries the pending downloads. All downloads that have not
finished yet are listed separately with this command. You can use few
search options and actions:

Each search option that takes an argument is compared against the
corresponding field. You can use % character as a wildcard (matching
many characters).

Actions are then applied to each file in the result. Actions are

- list (the default): print it to stdout
- fetch: resume the download. This can be used if a download has been
  cancelled. Note that it does *not* check whether a current process
  is running regarding this download. It just starts a new one.
- clear: deletes the pending download entry from the db

Action names can be abbreviated to the shortest non-ambiguous
name. Only one action can be given at a time."
  (let* ((action-fn (%find-query-action action
                                        *pending-actions-alist*
                                        *pending-actions-argn*
                                        "list"))
         (filter (if (empty? limit) #'identity (%query-filter-limit (as-int limit)))))
    (unless noheader
      (let ((searchstr (non-empty (%search-params-str `("keep" ,qkeep) source limit))))
        (format t "Looking for ~@[~a~]~:[all~;~]~%" searchstr searchstr)))
    (dlm-query action-fn
               `(:keep ,qkeep :source ,source)
               :filter filter
               :table "dlm_pending")))

(defun %info-value-string (value key)
  (cond ((member key '(:keep :valid))
         (format nil "~:[No~;Yes~]" value))
        ((member key '(:time :lastmod))
         (format nil "~a" (get-time-string value)))
        ((member key '(:lifetime))
         (format nil "~a" (format-duration value)))
        ((member key '(:length))
         (format nil "~a" (format-bytes value)))
        (t
         (format nil "~a" value))))

(defun %info-print-metadata (info raw &optional s)
  (flet ((keyval (fmt key value)
           (format s fmt key (if raw value
                                 (%info-value-string value key)))))
    (let* ((checks (getf info :checks))
           (meta (getf info :db-metadata))
           (valid? (getf info :metadata-valid?))
           (keys (delete :location (remove-if-not #'keywordp meta))))
      (keyval "~&~@(~a~): ~a~%" :location (getf meta :location))
      (keyval "~&~@(~a~): ~a~%" :valid valid?)
      (dolist (key keys)
        (let ((check (find-if (lambda (el) (eql (getf el :name) key))
                              checks)))
          (keyval "~&~@(~a~): ~a" key (getf meta key))
          (when (and (not valid?) check)
            (keyval " (~@a: ~a)" "expected" (getf check :expected)))
          (format s "~%")))
      (format s "~%"))))

(defcommand info (raw-values &free files)
  "Show information about downloaded files.

For a given file the information from the database is displayed in
key-value form."
  (let ((failures?))
    (dolist (file files)
      (let ((info (dlm-info file)))
        (if (getf info :error)
            (progn
              (setq failures? t)
              (echo "~a: ~a" file (getf info :msg)))
            (%info-print-metadata info raw-values t))))))



(defun main (&rest args)
  (let ((argv (or args (command-line-args)))
        (*package* (find-package :dlm-cli)) ;; when running from image, this is :cl-user
        (configfile (merge-pathnames #p".config/dlm/config.lisp"
                                     (user-homedir-pathname))))
    (handler-bind ((user-error #'(lambda (c)
                                   (echo "~a" (user-error-msg c))
                                   (system-exit 1)))
                   (error #'(lambda (c)
                              (if (getenv "DLM_DEBUG")
                                  (error c)
                                  (progn
                                    (echo "Ooops. An internal error occured. This is a bug!")
                                    (echo "~a" c)
                                    (system-exit 2))))))
      (let ((cfgfile (getenv "DLM_CONFIG")))
        (when (and cfgfile (not (probe-file cfgfile)))
          (user-error "Custom config file not found: ~a" cfgfile))
        (when (probe-file (or cfgfile configfile))
          (load (or cfgfile configfile)))
        (cli-main argv *default-command*)))))
