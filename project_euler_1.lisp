(defun euler-1 (n)
  (cond
	((zerop n) 0)
	((or (zerop (rem n 3)) (zerop (rem n 5))) (+ n (euler-1 (- n 1))))
	(t (euler-1 (- n 1)))
	))
(format t "~d ~%" (euler-1 1000))
