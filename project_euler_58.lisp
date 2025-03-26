(defun euler-58 ()
  (flet ((is-prime (N)
					(cond
					  ((= N 2) T)
					  ((= N 3) T)
					  ((evenp N) NIL)
					  (T (let ((d 3))
						   (loop
							 (if (zerop (rem N d)) (return NIL))
							 (incf d 2)
							 (when (> (* d d) N) (return T))))))))
		 (let ((side 3) (p 0) (v 1) (n 1))
		   (loop do
				 (let ((l (1- side)))
				   (loop for i from 1 to 4 do
						 (if (is-prime (incf v l))
						   (incf p)))
				   (incf n 4))
				 (if (< (/ p n) 0.1) (return-from euler-58 side))
				 (incf side 2)))))

(format t "~A~%" (euler-58))
