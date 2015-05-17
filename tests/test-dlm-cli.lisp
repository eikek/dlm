(in-package :dlm-cli-test)
(in-suite :dlm-cli-suite)

(test with-option-macro
  (is (equal
       (macroexpand-1
        '(dlm-cli::with-options '(argv-list)
          (keep &parameters target)))
       '(DLM-CLI::WITH-CLI-OPTIONS '(ARGV-LIST)
         ((KEEP ((#\k "keep") NIL "Flag the file to never be deleted by dlm"))
          &PARAMETERS (TARGET ((#\t "target") "./" "The target directory")))))))


(set-options
  `( ;; bool-options
  keep    ((#\k "keep") nil "Flag the file to never be deleted by dlm")
  target  ((#\t "target") "./" "The target directory")))

(defcommand fetchfile (keep &parameters target &free url)
  (list :keep keep :target target :url url))

(test defcommand-macro
  (is (equal
       (fetchfile '("--keep" "--target" "/bla" "myurl"))
       '(:keep t :target "/bla" :url ("myurl")))))

(test get-action-by-prefix
  (is (equalp '(("shell" . 1) ("shore" . 2))
              (dlm-cli::get-action-by-prefix
               "sh" '(("shell" . 1) ("shore" . 2) ("ying" . 3)))))
  (is (equalp '(("shell" . 1))
              (dlm-cli::get-action-by-prefix
               "she" '(("shell" . 1) ("shore" . 2) ("ying" . 3)))))
  (is  (equalp nil
               (dlm-cli::get-action-by-prefix
                "nix" '(("shell" . 1) ("shore" . 2) ("ying" . 3)))))
  (is (equal "fetch" (caar (dlm-cli::get-action-by-prefix
                            "fe" dlm-cli::*query-actions-alist*)))))

(test find-query-action-errors
  (is (equal t (handler-case
                   (progn
                     (dlm-cli::%find-query-action
                      '("move") dlm-cli::*query-actions-alist* dlm-cli::*query-actions-argn*)
                     nil)
                 (user-error ()
                   t))))
  (is (equal nil
             (handler-case
                 (progn
                   (dlm-cli::%find-query-action
                    '("move" "/a") dlm-cli::*query-actions-alist* dlm-cli::*query-actions-argn*)
                   nil)
               (user-error ()
                 t))))
  (is (equal t
             (let ((argns `(("move" . ,(lambda (args) (> (length args) 2))))))
               (and (dlm-cli::%find-query-action
                     '("move" "1" "2" "3") dlm-cli::*query-actions-alist* argns)
                    (handler-case
                        (progn
                          (dlm-cli::%find-query-action
                           '("move" "1") dlm-cli::*query-actions-alist* argns)
                          nil)
                      (user-error () t)))))))
