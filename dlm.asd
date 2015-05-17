(in-package :asdf-user)

(defsystem :dlm
  :description "A convenient wrapper around download programs."
  :version "0.0.1"
  :author "Eike Kettner <eike.kettner@posteo.de>"
  :entry-point "dlm-cli:main"
  :licence "GPLv3"
  :depends-on (:unix-options :ironclad :external-program :sqlite)
  :pathname "src"
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "db" :depends-on ("utils"))
               (:file "dlm" :depends-on ("db" "utils" "package"))
               (:file "cli-support" :depends-on ("utils" "package"))
               (:file "cli" :depends-on ("dlm" "utils" "cli-support" "package")))
  :in-order-to ((test-op (load-op :dlm-test)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN-DLM-TESTS" :dlm-util-test))))

(defsystem :dlm-test
  :description "Tests for dlm."
  :licence "GPLv3"
  :depends-on (:dlm :fiveam)
  :pathname "tests"
  :components ((:file "suite")
               (:file "test-utils" :depends-on ("suite"))
               (:file "test-dlm-cli" :depends-on ("suite"))))
