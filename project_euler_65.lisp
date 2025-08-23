(defun euler-65 ()
  (flet ((sum-digits (n)
		   (loop for x = n then (floor x 10)
				 while (> x 0) sum (rem x 10))))

	(let ((e-cf (list 2)))
	  (loop for k from 1 to 33 do (nconc e-cf (list 1 (* 2 k) 1)))
	  (loop for a in e-cf
			for p0 = 0 then p1
			for p1 = 1 then p2
			for p2 = (+ (* a p1) p0)
			finally (return-from euler-65 (sum-digits p2))))))

(format t "~D~%" (euler-65))
