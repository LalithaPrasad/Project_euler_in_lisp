(defun collatz-len (n &optional (cnt 0))
  (cond
	((= n 1) cnt)
	((oddp n) (collatz-len (+ (* 3 n) 1) (+ cnt 1)))
	(t (collatz-len (/ n 2) (+ cnt 1)))))

(defun euler-14 (&optional (n 1) (maxl 0) (mxn 0))
  (if (>= n 1000000)
	(values mxn maxl)
	(let ((l (collatz-len n)))
	  (if (> l maxl) (euler-14 (+ n 1) l n)
		(euler-14 (+ n 1) maxl mxn)))))

(multiple-value-bind (a b) (euler-14)
    (format t "~A ~A~%" a b)
)
