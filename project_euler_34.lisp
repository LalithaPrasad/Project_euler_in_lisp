(defun euler-34 ()
  (let ((factorials '(1 1 2 6 24 120 720 5040 40320 362880)))
	(labels ((sum-facts (n &optional (s 0))
						(if (= n 0)
						  s
						  (sum-facts (floor n 10) (+ s (nth (rem n 10) factorials))))))
	  (loop for n from 10 to 1000000
			when (= n (sum-facts n)) sum n))))

(format t "~d~%" (euler-34))
