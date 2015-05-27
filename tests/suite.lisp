(defpackage :dlm-util-test
  (:use :fiveam :cl :dlm-util)
  (:export run-dlm-tests))

(in-package :dlm-util-test)
(def-suite :dlm-util-suite :description "Tests for dlm-util")

(defun run-dlm-tests (&optional exit)
  "Runs all tests. If EXIT is true, exit lisp with return code
depending on test result."
  (let ((pkgs '((:dlm-util-test . :dlm-util-suite)
                (:dlm-test . :dlm-suite)
                (:dlm-cli-test . :dlm-cli-suite))))
    (flet ((runtest (suite)
             (let* ((result (fiveam:run suite))
                    (success (fiveam:results-status result)))
               (unless success
                 (fiveam:explain! result))
               success)))
      (let ((status
             (reduce (lambda (val pkg)
                       (let* ((*package* (find-package (car pkg))))
                         (and val (runtest (cdr pkg)))))
                     pkgs
                     :initial-value t)))
        (when exit
          (system-exit (if status 0 1)))
        (unless status
          (error "tests failed"))
        status))))

(defpackage :dlm-cli-test
  (:use :cl :fiveam :dlm-cli :dlm-util :dlm :unix-options))

(in-package :dlm-cli-test)
(fiveam:def-suite :dlm-cli-suite :description "Tests for dlm-cli")


(defpackage :dlm-test
  (:use :cl :fiveam :sqlite :cl-ansi-text :dlm-util :dlm))

(in-package :dlm-test)
(fiveam:def-suite :dlm-suite :description "Tests for dlm.lisp")
