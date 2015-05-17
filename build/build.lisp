(in-package :cl-user)
(require 'asdf)

(asdf:initialize-source-registry
 `(:source-registry
   (:directory ,(if (probe-file "dlm.asd")
                    (truename ".")
                    (truename (merge-pathnames ".."))))
   :inherit-configuration))

(defun system-exit (&optional (code 0))
  #+sbcl (sb-ext:exit :code code)
  #+clisp (ext:exit code)
  #+ccl (ccl:quit code)
  #+allegro (excl:exit code)
  #- (or sbcl clisp ccl allegro)
  (error "system-exit not defined for this lisp impl."))

(defun load-dlm ()
  (asdf:oos 'asdf:load-op 'dlm))

(load-dlm)

(defun compile-dlm ()
  (asdf:oos 'asdf:compile-op :dlm))

(defun test-dlm ()
  "Run the tests and return true for success and nil for failure. More
information is printed to stdout durint the run."
  (asdf:oos 'asdf:load-op 'dlm-test)
  (handler-case
      (progn (asdf:oos 'asdf:test-op :dlm) t)
    (error () nil)))

(defun test-dlm-and-exit ()
  (let ((result (test-dlm)))
      (system-exit (if result 0 1))))

(defun make-image ()
  (let* ((version (asdf:component-version (asdf:find-system :dlm)))
         (program (format nil "dlm-~a" version)))
    #+sbcl
    (sb-ext:save-lisp-and-die program
                              :toplevel #'dlm-cli:main
                              :executable t)

    #+ccl
    (ccl:save-application program
                          :prepend-kernel t
                          :toplevel-function #'dlm-cli:main)

    #+clisp
    (ext:saveinitmem program
                     :init-function #'dlm-cli:main
                     :executable t
                     :keep-global-handlers t
                     :norc t
                     :documentation "The dlm executable")))
