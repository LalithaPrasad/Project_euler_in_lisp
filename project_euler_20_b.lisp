(defun fact (n)
  (if (< n 2)
	1
	(* n (fact (- n 1)))))

(defun euler-20 (n)
  (if (zerop n)
	0
	(+ (rem n 10) (euler-20 (floor n 10)))))

(format t "~A~%" (euler-20 (fact 100)))
