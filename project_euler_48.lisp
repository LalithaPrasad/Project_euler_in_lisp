(defun euler-48 ()
  (labels ((sum-self-powers (n)
							(if (zerop n)
							  0
							  (+ (expt n n) (sum-self-powers (- n 1))))))
	(let ((s (write-to-string (sum-self-powers 1000))))
	  (subseq s (- (length s) 10)))))

(format t "~A~%" (euler-48))
