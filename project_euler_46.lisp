(defun is-prime (n)
  (labels ((helper (m k)
				   (let ((x (- (* 6 k) 1)) (y (+ (* 6 k) 1)))
					 (cond
					   ((> x m) t)
					   ((zerop (rem n x)) nil)
					   ((> y m) t)
					   ((zerop (rem n y)) nil)
					   (t (helper m (+ k 1)))))))

	(cond
	  ((< n 2) nil)
	  ((= n 2) t)
	  ((= n 3) t)
	  ((zerop (rem n 2)) nil)
	  ((zerop (rem n 3)) nil)
	  (t (helper (isqrt n) 1)))))

(defun euler-46 (n)
  (labels ((helper (n i)
				   (if (< i n)
					 (if (is-prime (- n (* 2 i i)))
					   nil
					   (helper n (+ i 1)))
					 t)))
	(if (is-prime n)
	  (euler-46 (+ n 2))
	  (if (helper n 1)
		n
		(euler-46 (+ n 2))))))

(format t "~A~%" (euler-46 3))
