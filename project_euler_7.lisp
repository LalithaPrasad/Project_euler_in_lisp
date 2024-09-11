(defun nth-prime (m)
  (labels ((is-prime (n)
					 (cond
					   ((= n 2) T)
					   ((= n 3) T)
					   ((evenp n) NIL)
					   (T (let ((d 3))
							(loop
							  (if (zerop (rem n d))
								(return NIL))
							  (incf d 2)
							  (when (> (* d d) n) (return T))))))))
	(let ((x 2) (count 0))
	  (loop
		(if (is-prime x)
		  (incf count))
		(when (= count m) (return x))
		(incf x)))))

(format t "~A~%" (nth-prime 10001))
