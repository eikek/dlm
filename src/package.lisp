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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :unix-options)
    (require 'unix-options))
  (unless (find-package :sqlite)
    (require 'sqlite)))

(defpackage :dlm-util
  (:use :cl)
  (:export *dlm-version*
           getenv
           command-line-args
           string-split
           concat
           subseq-after
           subseq-before
           when-let
           plist-replace
           ->
           cond->
           with-current-dir
           file-atime
           file-atime-since
           curry
           empty?
           non-empty
           format-duration
           format-bytes
           parse-duration
           get-time-string
           conjunct
           echo
           system-exit
           run-hooks
           user-error
           user-error-msg
           starts-with?
           as-int))

(defpackage :dlm
  (:use :cl :sqlite :cl-ansi-text :dlm-util)
  (:export *database*
           *fetch-default-bin*
           *fetch-default-args*
           *file-lifetime*
           *default-target*
           *which-program*
           *download-programs-alist*
           *youtube-dl-bin*
           *youtube-dl-args*
           *download-notify-hook*
           file-metadata-valid?
           add-fetch-config
           program-exists?
           dlm-local-source?
           dlm-download
           dlm-add-local
           dlm-fetch
           dlm-redownload
           dlm-delete-file
           dlm-clear-pending
           dlm-clear-metadata
           dlm-query
           dlm-change-metadata
           dlm-format-metadata
           dlm-print-metadata
           dlm-move
           dlm-prune
           dlm-collect-garbage
           dlm-info))

(defpackage :dlm-cli
  (:use :cl :dlm :dlm-util :unix-options)
  (:export set-options
           *default-command*
           *query-actions-alist*
           *query-actions-argn*
           *pending-actions-alist*
           *pending-actions-argn*
           defcommand
           cli-main
           main))
