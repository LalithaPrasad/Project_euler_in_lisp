(defun euler-4 ()
  (labels ((reverse-number (n)
						   (let ((r 0))
							 (loop while (> n 0) do
								   (setf r (+ (* r 10) (rem n 10)) n (floor n 10)))
							 r)))
	(loop for x from 101 below 1000 maximize
		  (loop for y from x below 1000
				for p = (* x y)
				when (= p (reverse-number p)) maximize p))))

(format t "~A~%" (euler-4))
