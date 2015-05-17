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

(in-package :dlm-util)

(defvar *dlm-version*
  (asdf:component-version (asdf:find-system :dlm)))

(defun getenv (name &optional default)
  "Get the value of an environment variable. Copied from
http://cl-cookbook.sourceforge.net/os.html. "
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))

(defun command-line-args ()
  "Return the argument strings list across multiple
implementations. Copied from
http://cl-cookbook.sourceforge.net/os.html."
  (or
   #+SBCL sb-unix::*posix-argv*
   #+LISPWORKS system:*line-arguments-list*
   #+clisp (cons "dlm" *args*)
   #+clozure (ccl::command-line-arguments)
   #+CMU extensions:*command-line-words*
   nil))


(defun string-split (delim-char str)
  "Split STR on any number of DELIM-CHAR characters into a list of
strings. DELIM-CHAR can be a single char or a list thereof."
  (let* ((delims (if (listp delim-char) delim-char (list delim-char)))
         (trimmed (string-trim delims str))
         (prefix (loop
                   for c across trimmed
                   until (find c delims)
                   collect c)))
    (if prefix
        (cons (coerce prefix 'string)
              (string-split delims (subseq trimmed (length prefix))))
        '())))

(defun concat (&rest strings)
  "Concats all given strings"
  (apply #'concatenate (cons 'string (mapcar #'string strings))))


(defun subseq-after (item sequence &optional without-itemp)
  "Return the subsequence of SEQUENCE after ITEM."
  (let ((pos (position item sequence)))
    (when pos
      (if without-itemp
          (subseq (subseq sequence pos) 1)
          (subseq sequence pos)))))

(defun subseq-before (item sequence &optional with-last-p)
  (let ((pos (position item sequence)))
    (when pos
      (subseq sequence 0 (if with-last-p
                             (1+ pos)
                             pos)))))

(defmacro when-let ((var form) &body body)
  `(let ((,var ,form))
     (when ,var
       ,@body)))

(defun plist-replace (plist key value)
  "Replace the the value of KEY in PLIST with the new VALUE. Return a
new plist."
  (if (find key plist)
      (append
       (subseq-before key plist)
       (list key value)
       (cdr (subseq-after key plist t)))
      plist))

(defmacro -> (init &body forms)
  "The threading macro from clojure.

Threads the expr through the forms. Inserts INIT as the second item in
the first form, making a list of it if it is not a list already*. If
there are more forms, inserts the first form as the second item in
second form, etc.

*) automatic list creation not yet done here"
  (reduce (lambda (val form)
            (let ((head (car form))
                  (tail (cdr form)))
              (cons head (cons val tail))))
          forms
          :initial-value init))

(defmacro cond-> (init &body forms)
  "The cond-> from clojure.

Takes an INIT expression and a set of test/form pairs. Threads
expr (via ->) through each form for which the corresponding test
expression is true. Note that, unlike cond branching, cond-> threading
does not short circuit after the first true test expression."
  (reduce (lambda (val form)
            (let* ((expr (cadr form))
                   (head (car expr))
                   (tail (cdr expr)))
              `(if ,(car form) ,(cons head (cons val tail)) ,val)))
          forms
          :initial-value init))

(defmacro with-current-dir (dirname &body body)
  "Change current directory to DIRNAME and evaluate BODY. Reset
current directory after BODY has been evaluated. If DIRNAME is nil,
evaluate the body without changing current directory."
  (let ((curdir (gensym)))
    `(if ,dirname
         (let ((curdir *default-pathname-defaults*)
               (*default-pathname-defaults* (truename ,dirname)))
           #+sbcl (sb-posix:chdir ,dirname)
           #+clisp (cd ,dirname)
           (unwind-protect
                (progn
                  ,@body)
             #+sbcl (sb-posix:chdir curdir)
             #+clisp (cd curdir)))
         (progn
           ,@body))))

(defun file-atime (filename)
  "Return last access time of FILENAME in common lisp universal time."
  (let* ((file (probe-file filename))
         (atime (when file
                  #+clisp (posix:atime (posix:file-stat file))
                  #+sbcl (sb-posix:stat-atime (sb-posix:stat file)))))
    (+ (encode-universal-time 0 0 0 1 1 1970 0)
       atime)))

(defun file-atime-since (filename)
  "Return the seconds from now FILENAME has been last accessed."
  (when-let (atime (file-atime filename))
    (- (get-universal-time) atime)))

(defun curry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))


(defun empty? (seq)
  "Return T if SEQ is empty, otherwise F."
  (= 0 (length seq)))

(defun non-empty (seq)
  "Return SEQ if it is non-empty. Otherwise return nil."
  (and (> (length seq) 0) seq))


(defparameter %duration-conv
  '((#\s . 60) (#\M . 60) (#\h . 24) (#\d . 30) (#\m . 12) (#\y . 1)))

(defparameter %size-conv
  '((:bytes . 1024) (:kb . 1024) (:mb . 1024) (:gb . 1)))

(defun %format-measure (num unit table fmt)
  (let* ((cur (assoc unit table))
         (next (second (member cur table :test #'equalp)))
         (amount (if next (/ num (cdr cur)))))
    (unless cur
      (error "Wrong unit ~a, available are: ~{~a~^, ~}" unit (mapcar #'first table)))
    (if (and next (>= amount 1))
        (%format-measure amount (first next) table fmt)
        (format nil fmt num unit))))

(defun format-duration (secs &optional (unit #\s) (fmt "~5,1f~a"))
  (%format-measure secs unit %duration-conv fmt))

(defun format-bytes (num &optional (unit :bytes) (fmt "~5,1f~(~a~)"))
  (%format-measure num unit %size-conv fmt))

(defun parse-duration (dur &optional (table '((#\m . 2592000) (#\d . 86400) (#\h . 3600) (#\M . 60))) (result 0))
  "Parses a simple duration string into seconds. The string should
  look like this: \"2d10h\". Supports m,d,h and M."
  (if table
      (let* ((cons (car table))
             (parts (string-split (car cons) dur)))
        (if (and (= 1 (length parts)) (string= dur (first parts)))
            (parse-duration dur (cdr table) result)
            (parse-duration (second parts)
                            (cdr table)
                            (+ result (* (parse-integer (first parts)) (cdr cons))))))
      result))

(defun get-time-string (universal-time)
  (multiple-value-bind
        (second minute hour date month year)
      (decode-universal-time universal-time 0)
    (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month date hour minute second)))

(defun conjunct (&rest fns)
  "Create a function that delegates to the specified functions
  returning the first NIL value or T if all given functions return
  non-nil."
  (lambda (&rest args)
    (reduce (lambda (val f)
              (and val
                   (apply f args)))
            fns
            :initial-value t)))

(defun echo (fmt &rest args)
  "Short for `(format t fmt args)'."
  (apply #'format t (concat fmt "~%") args))


(defun system-exit (&optional (code 1))
  #+sbcl (sb-ext:exit :code code)
  #+clisp (ext:exit code)
  #+ccl (ccl:quit code)
  #+allegro (excl:exit code)
  #- (or sbcl clisp ccl allegro)
   (error "system-exit not defined for this lisp impl."))

(defun run-hooks (hook-var &rest args)
  "Call all functions in list HOOK-VAR with ARGS."
  (when hook-var
    (dolist (fn hook-var)
      (unless (functionp fn)
        (error "Not a function: ~a." fn))
      (handler-case
          (apply fn args)
        (error (e)
          (format t "Error in notify hook: ~a" e))))))

(define-condition user-error (error)
  ((text :initarg :text :reader user-error-msg)))

(defun user-error (text &rest args)
  (let ((msg (apply #'format nil text args)))
    (error 'user-error :text msg)))

(defun starts-with? (seq1 seq2)
  "Return true if SEQ1 is a prefix of SEQ2"
  (when (<= (length seq1) (length seq2))
    (let ((match (mismatch seq1 seq2)))
      (or (null match)
          (= match (length seq1))))))

(defun as-int (str)
  (handler-case
      (parse-integer str)
    (error (e)
      (user-error "~a is not a number" str))))
