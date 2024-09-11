(defun euler-3 (n)
  (labels ((helper (n pf pf-list)
				   (cond
					 ((> pf n) (reverse pf-list))
					 ((zerop (mod n pf)) (helper (/ n pf) pf (cons pf pf-list)))
					 (t (helper n (incf pf) pf-list)))))
	(helper n 2 nil)))

(dolist (x (euler-3 600851475143)) (format t "~a " x))
(terpri)
