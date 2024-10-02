(defun euler-30 ()
  (labels ((digit-sum (n p)
					  (if (= n 0)
						0
						(+ (expt (rem n 10) p) (digit-sum (floor n 10) p)))))
	(loop for n from 2 to 1000000 when (= n (digit-sum n 5)) sum n)))

(format t "~d~%" (euler-30))
