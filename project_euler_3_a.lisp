(defun euler-3 (n)
  (labels ((helper (n pf)
				   (cond
					 ((> pf n) pf)
					 ((zerop (mod n pf)) (helper (/ n pf) pf))
					 (t (helper n (incf pf))))))
	(helper n 2)))

(format t "~A~%" (euler-3 600851475143))
