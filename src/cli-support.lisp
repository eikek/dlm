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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *options* '()))

(defvar *commands* '())

(defun %candidates-string (prefix candidates)
  (with-output-to-string (s)
    (format s "The command '~a' is ambiguous.~%" prefix)
    (format s "~{- ~a~^~%~}" (mapcar #'car candidates))))

(defun find-command (name)
  "Find a command by name (either a symbol or a string)."
  (flet ((prefix? (cmd)
           (starts-with? (if (symbolp name)
                             (symbol-name name)
                             (string-upcase name))
                         (symbol-name cmd))))
    (let ((candidates
           (remove-if-not #'prefix? *commands* :key #'car)))
      (cond ((empty? candidates) nil)
            ((= 1 (length candidates)) (car candidates))
            (t (user-error (%candidates-string name candidates)))))))

(defun %make-short-help (pair)
  (let ((text (cdr pair)))
    (cons (car pair)
          (or (subseq-before #\Newline text)
              (subseq-before #\. text t)
              (concat (subseq text 0 (min (length text) 25)) "...")))))

(defun find-short-help (name)
  "CMD is the `(cmd . help-text)' where the help text is shortened
until the first newline."
  (let ((cmd (find-command name)))
    (%make-short-help cmd)))

(defmacro set-options (defs)
  "Define a global set of option definitions. This is a plist where
each key must be a symbol which can be used in `defcommand' macros to
refer to each option. The value to each symbol is a option definiton
as defined in `unix-options' package. Please see its documentation for
more information.

Example:
    (set-options
       ;; bool-options
       verbose ((#\v nil) nil \"Be more verbose.\")
       keep    ((#\k \"keep\") nil \"Flag the file to never be deleted by dlm\")

       ;; parameters
       target  ((#\t \"target\") \"./\" \"The target directory\"))

There are bool-options and parameter-options, the former take no
arguments but the latter do.

With this definitions a command can be implemented like this:

    (defcommand add (keep &parameters target &free files)
      (format t \"Add file ~a to ~a and and keep: ~:[no~;yes~].\" files target keep))

Then running your program from the shell

    $ myprogram add --target /a/path /file1 /file2
    Add file (/file1 /file2) to /a/path and and keep: no.

    $ myprogram add -k /file3
    Add file (/file3) to ./ and and keep: yes.
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *options*
           (,@defs))))

(defmacro with-options (argv optdefs &body body)
  "A macro, such that

    (with-options (argv-list) t)
        (debug &parameters target &free url)
      (list :debug debug :target target :url (car url)))

becomes:

    (with-cli-options (argv-list t)
        ((debug ((#\d \"debug\") nil \"Run in debug mode\"))
         &parameters (target ((#\t \"target\") \"./\" \"The target directory\"))
         &free url)
      (list :debug debug :target target :url (car url)))

where the option definition is taken from the global var *options*."
  (flet ((translate (def-list)
           (loop
              :for x :in def-list
              :for free = (or free (eq x 'unix-options:&free))
              :for param = (eq x 'unix-options:&parameters)
              :collect
                (if (or free param) x
                    `(,x ,(or (getf *options* x)
                              (error (format nil "Unknown option definition for: ~a" x))))))))
    `(unix-options:with-cli-options ,argv
         ,(translate optdefs)
       ,@body)))

(defmacro defcommand (name options &body body)
  "Define a funcition that expects a command line argument list as input.

OPTIONS define all valid options for this command. It must be
compatible to the `with-options' macro. That is, first specify a list
of bool-options (those not taking arguments), then provide the
`&paramters' symbol following a list of parameter-options (those that
take an argument). After that you can add `&free' symbol following one
binding to hold all arguments not already bound to previous symbols in
a list.

All options must be drawn from the global `*options*' property list.

A DOCSTR can be supplied after OPTIONS, that if specified, is attached
to the command and displayed to the user if the help screen for this
command is requested."
  (let* ((argv (gensym))
         (docs (if (stringp (car body)) (car body) "Not documented."))
         (code (if (stringp (car body)) (cdr body) body))
         (item (cons name docs))
         (free (subseq-after 'unix-options:&free options t))
         (usage (concatenate 'string "Usage: dlm [--]"
                             (string-downcase (string name))
                             " [OPTIONS] "
                             (if free (apply #'concat (mapcar #'string free)) "")
                             "~&~%Options are:~%~@{~A~%~}~%")))
    (when (> (length free) 1)
      (error (format nil "More than one free variable is not allowed: ~a" free)))
    `(progn
       (setq *commands* (cons ',item
                              (remove-if (lambda (pair) (eq (car pair) ',name))
                                         *commands*)))
       (defun ,name (,argv)
          (with-options (,argv ,usage) ,options
                        ,@code)))))


(defun split-argv (argv &optional default-command)
  "Turn the given argument list ARGV into a list where the first
component is the command (a string) and the other the list of
options (the rest of ARGV, strings). It is assumed that the first
element is the program name itself, which is discarded. Return NIL if
ARGV is only one element or nil."
  (let* ((cmd (and argv
                  (> (length argv) 1)
                  (string-left-trim "-" (second argv))))
         (res (or cmd default-command)))
    (when res
      (cons res (cddr argv)))))

(defun call-command (name argv)
  "Looks up a command by NAME and calls it with the argument list
ARGV. If the command is not found, a message is printed and the global
help is shown."
  (let ((cmd (find-command name)))
    (if cmd
        (funcall (car cmd) argv)
        (user-error "Error: command '~a' unknown." name))))

(defcommand help (&free command)
  "Shows a short help for this program.

If called without arguments, a little help is shown, listing all
commands with a short description. If a command is given, a more
detailed help to this command is shown (if provided)."
  (when (> (length command) 1)
    (echo "Warning: You can only get help for one command at a time."))
  (unless command
    (echo "dlm ~a~%" *dlm-version*)
    (echo "dlm is a download manager that keeps track of what has been")
    (echo "downloaded but delegates downloading to good tools like curl.")
    (echo "Commands may be abbreviated to the shortest non-ambiguous")
    (echo "name.~%")
    (echo "Usage: dlm command [options] [...]~%")
    (echo "Commands: ~%~{  ~a~%~}"
          (mapcar (lambda (pair)
                    (format nil "~(~a~): ~a" (car pair) (cdr pair)))
                  (mapcar #'%make-short-help *commands*))))
  (when command
    (let ((cmdcons (find-command (car command))))
      (when (car cmdcons)
        (echo "The '~(~a~)' command~%" (car cmdcons)))
      (call-command (car command) '("-h"))
      (when (cdr cmdcons)
        (echo "~a~%" (cdr cmdcons))))))

(defun cli-main (argv &optional default-command)
  "ARGV is an argument list given on the command line. Finds the
command and executes with its options."
  (when default-command
    (unless (find-command default-command)
      (error "Default command unknown: ~a" default-command)))
  (let* ((cmd (split-argv argv default-command))
         (cmdname (first cmd)))
    (when cmd
      (let ((cmdcons (find-command cmdname)))
        (if cmdcons
            (call-command (first cmdcons) (cdr cmd))
            (if default-command
                (call-command default-command (cdr argv))
                (echo "Error: Command unknonw: ~a" cmdname)))))
    (unless cmd
      (echo "Error: You must specify a command.")
      (help nil))))
