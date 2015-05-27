(in-package :dlm-test)
(in-suite :dlm-suite)

(defmacro with-file (file &body body)
  (let ((s (gensym)))
    `(progn
       (with-open-file (,s ,file :direction :output)
         (format ,s "hello world.\n"))
       (unwind-protect
           (progn
             ,@body)
         (delete-file ,file)))))

(defmacro with-testdb (&body body)
  (let ((name (gensym)))
    `(let* ((,name (merge-pathnames (string (gensym)) (merge-pathnames "target/")))
            (dlm::*database* (namestring ,name)))
       (unwind-protect
            (progn
              ,@body)
         (delete-file ,name)
         (uiop:delete-empty-directory "target")))))


(test add-local-files
  (with-file "test-download.txt"
    (let* ((meta (with-testdb (dlm::dlm-add-local "test-download.txt" :keep t))))
      (is (equal (getf meta :source) "test-download.txt"))
      (is (equal (getf meta :keep) t))
      (is (equal (getf meta :redownloads) 0))
      (is (equal (getf meta :lifetime) dlm::*file-lifetime*))
      (is (<= (getf meta :lastmod) (get-universal-time)))
      (is (= (getf meta :length) 13))
      (is (<= (getf meta :time) (get-universal-time)))
      (is (equal (getf meta :location)
                 (namestring (merge-pathnames "test-download.txt"))))
      (is (string= (getf meta :sha256)
                   "00758616cd62b995f91e79198dd72cd7fc169d2aef79e373dc223b471bf33f8a")))))
