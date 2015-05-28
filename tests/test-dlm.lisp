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

(test is-local-meta
  (with-file "testfile.txt"
    (let ((meta (dlm::make-download-metadata "testfile.txt" :source "testfile.txt")))
      (is (equal t (dlm::dlm-local-source? meta))))))

(test format-local-meta
  (with-file "testfile.txt"
    (let* ((meta (dlm::make-download-metadata "testfile.txt" :source "testfile.txt"))
           (str (string-split #\Space (dlm-format-metadata meta))))
      (is (= (length str) 6))
      (is (string= (nth 0 str) "[.e]["))
      (is (string= (nth 1 str) "0]"))
      (is (string= (nth 2 str) "1.0m"))
      (is (string= (nth 3 str) "13.0bytes"))
      (is (string= (nth 4 str) (namestring (truename "testfile.txt"))))
      (is (string= (nth 5 str) "[35m[-][0m")))))

(test dlm-local-source
  (with-file "testfile.txt"
    (let ((meta (dlm::make-download-metadata "testfile.txt"
                                             :source "https://www.youtube.com/watch?v=-123M")))
      (is (eq nil (dlm::dlm-local-source? meta))))))

(test dlm-info
  (is (equalp '(:error :file-not-found :msg "The file does not exist.")
              (dlm-info "blups")))
  (with-file "afile"
    (is (equalp '(:error :file-unknown :msg "The file is not known to dlm.")
                (dlm-info "afile"))))
  (with-file "test-download.txt"
    (with-testdb
      (dlm::dlm-add-local "test-download.txt" :keep t)
      (let* ((info (dlm-info "test-download.txt"))
             (fmeta (getf info :file-metadata))
             (dmeta (getf info :db-metadata)))
        (is (eq (getf info :metadata-valid?) t))
        (is (eq (getf info :checks) nil))
        (is (= 13 (getf fmeta :length)))
        (is (string= (namestring (truename "test-download.txt"))
                     (getf fmeta :location)))
        (is (string= "00758616cd62b995f91e79198dd72cd7fc169d2aef79e373dc223b471bf33f8a"
                     (getf fmeta :sha256)))
        (is (string= "test-download.txt"
                     (getf dmeta :source)))
        (is (eq t (getf dmeta :keep)))
        (is (= 0 (getf dmeta :redownloads)))
        (is (= dlm::*file-lifetime* (getf dmeta :lifetime)))))))
