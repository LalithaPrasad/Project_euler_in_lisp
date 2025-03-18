(defun euler-56 ()
  (labels ((sum-digits (n)
					   (if (zerop n)
						 0
						 (+ (rem n 10) (sum-digits (floor n 10))))))
	(loop for a from 2 below 100 
		  maximize (loop for b from 2 below 100
						 maximize (sum-digits (expt a b))))))

(format t "~A~%" (euler-56))
