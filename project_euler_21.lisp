(defun euler-21 ()
  (labels ((sf (n) (1+ (apply #'+ (loop for f from 2 to (isqrt n)
										when (zerop (rem n f)) collect (+ f (floor n f)))))))
	(apply #'+ (loop for n from 200 below 10000
					 when (and (= n (sf (sf n))) (not (= n (sf n)))) collect n))))

(format t "~A~%" (euler-21))
