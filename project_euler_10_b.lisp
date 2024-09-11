(defun euler-10 ()
  (labels ((is-prime (N)
					 (cond
					   ((= N 2) T)
					   ((= N 3) T)
					   ((evenp N) NIL)
					   (T (let ((d 3))
							(loop
							  (if (zerop (rem N d)) (return NIL))
							  (incf d 2)
							  (when (> (* d d) N) (return T))))))))
	(loop for x from 2 below 2000000 when (is-prime x) sum x)))

(format t "~A~%" (euler-10))
