(defun euler-55 ()
  (labels ((reverse-num (n &optional (r 0))
					  (if (zerop n)
						r
						(reverse-num (floor n 10) (+ (* r 10) (rem n 10)))))
		   (lychrel-nump (n)
						 (loop for count from 0 to 50 do
							   (let ((m (+ n (reverse-num n))))
								 (if (= m (reverse-num m))
								   (return-from lychrel-nump nil)
								   (setf n m))))
						 t))
	(loop for n from 1 below 10000
		  count (lychrel-nump n))))


(format t "~A~%" (euler-55))
