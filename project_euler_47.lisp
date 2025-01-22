(defun dpf (n)
  (labels ((helper (n x pl)
				   (cond
					 ((< n x) pl)
					 ((zerop (rem n x)) (helper (floor n x) x (if (member x pl) pl (cons x pl))))
					 (t (helper n (+ x 1) pl)))))
	(helper n 2 '())))

(defun euler-47 (n)
  (cond
	((/= 4 (length (dpf (+ n 3)))) (euler-47 (+ n 4)))
	((/= 4 (length (dpf (+ n 2)))) (euler-47 (+ n 3)))
	((/= 4 (length (dpf (+ n 1)))) (euler-47 (+ n 2)))
	((/= 4 (length (dpf n))) (euler-47 (+ n 1)))
	(t n)))

(format t "~A~%" (euler-47 2))
