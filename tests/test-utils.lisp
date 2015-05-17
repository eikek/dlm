(in-package :dlm-util-test)
(in-suite :dlm-util-suite)

(test string-split
  (is (equal '("a" "b")
             (string-split #\Space "a b")))
  (is (equal '("a" "b")
             (string-split #\Space "  a   b  ")))
  (is (equal '("abc" "1231" "def")
             (string-split #\Space "abc  1231 def")))
  (is (equal '("a")
             (string-split #\e "a"))))

(test starts-with
  (is (equal t (starts-with? "ab" "abcd")))
  (is (equal nil (starts-with? "abcde" "ab")))
  (is (equal nil (starts-with? "abs" "abcd"))))

(test concat
  (is (equal "ab12" (concat "ab" "12")))
  (is (equal "a1b2c3e4f5" (concat "a1" "b" "2" "c3e" "4" "f5")))
  (is (equal "" (concat))))

(test subseq-after
  (is (equal "6" (subseq-after #\5 "123456" t)))
  (is (equal "56" (subseq-after #\5 "123456")))
  (is (equal '(a b c) (subseq-after 'a '(1 2 3 a b c))))
  (is (equal '() (subseq-after 'a '())))
  (is (equal '(a) (subseq-after 'a '(a))))
  (is (equal '() (subseq-after 4 '(1 6 8 9)))))

(test subseq-before
  (is (equal "12" (subseq-before #\3 "1234")))
  (is (equal "123" (subseq-before #\3 "1234" t)))
  (is (equal '() (subseq-before 1 '())))
  (is (equal '() (subseq-before 1 '(5 6 7)))))

(test when-let
  (is (equalp (macroexpand-1 '(when-let (x 1) x))
              '(let ((x 1))
                (when x x)))))

(test plist-replace
  (let ((pl1 '(:a 1 :b 2 :c 3)))
    (is (equalp '(:a 1 :b 5 :c 3)
                (plist-replace pl1 :b 5)))
    (is (equalp pl1
                (plist-replace pl1 :z 9)))
    (is (equalp pl1 '(:a 1 :b 2 :c 3)))))

(test macro->
  (is (equalp (macroexpand-1 '(-> x (+ 1) (+ 3)))
              '(+ (+ x 1) 3)))
  (is (equalp (macroexpand-1 '(-> x (+ 1)))
              '(+ x 1)))
  (is (equalp (macroexpand-1 '(-> x))
              'x)))

(test macro-cond->
  (is (equalp (macroexpand-1 '(cond-> x
                               ((> x 2) (+ 4))))
              '(if (> x 2) (+ x 4) X)))
  (is (equalp  (macroexpand-1 '(cond-> x
                                 ((> x 2) (+ 4))
                                 ((< x 6) (+ 5))))
                '(if (< X 6)
                  (+
                   (if (> X 2)
                       (+ X 4)
                       X)
                   5)
                  (if (> X 2)
                      (+ X 4)
                      X)))))

(test curry
  (let ((plus7 (curry #'+ 1 2 4)))
    (is (= 12 (funcall plus7 5)))
    (is (= 12 (apply plus7 1 2 '(2))))))

(test empty?
  (is (empty? ""))
  (is (not (empty? "a")))
  (is (empty? nil))
  (is (not (empty? '(1 2)))))

(test non-empty
  (is (equal '(1 2) (non-empty '(1 2))))
  (is (not (non-empty '())))
  (is (not (non-empty "")))
  (is (string= "ab" (non-empty "ab"))))

(test format-duration
  (is (equalp "5.0M" (format-duration 300 #\s "~,1f~(~a~)")))
  (is (equalp "1.0m" (format-duration 30.1 #\d "~,1f~(~a~)")))
  (is (equalp " 20.6h" (format-duration 1234 #\M))))

(test format-bytes
  (is (string= "1.2kb" (format-bytes 1234 :bytes "~,1f~(~a~)")))
  (is (string= "  1.0kb" (format-bytes 1024 :bytes)))
  (is (string= "533.8mb" (format-bytes 546630 :kb))))

(test parse-duration
  (= (parse-duration "2d10h")
     (+ (* 2 24 60 60) (* 10 60 60))))

(test conjunct
  (let ((conjfn (conjunct #'evenp #'plusp)))
    (is (funcall conjfn 10))
    (is (not (funcall conjfn 7)))
    (is (not (funcall conjfn -8)))))
