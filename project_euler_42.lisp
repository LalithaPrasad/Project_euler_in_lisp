(ql:quickload :cl-utilities)

(defun trngl-nums (s n)
  (if (> s 200)
	'()
	(let ((p (+ s n))) (cons p (trngl-nums p (1+ n))))))

(defun word-to-num (word)
  (let ((alphabets "-ABCDEFGHIJKLMNOPQRSTUVWXYZ-"))
	(loop for c across word
		  sum (position c alphabets))))

(defun euler-42 ()
  (labels ((input-42 () (cl-utilities:split-sequence #\,
													 (car (with-open-file (stream "words.txt")
															(loop for line = (read-line stream nil)
																  while line collect line))))))
	(let ((trn (trngl-nums 0 1)))
	  (loop for word in (input-42)
			count (member (word-to-num (string-trim "\"" word)) trn)))))

(format t "~A~%" (euler-42))

