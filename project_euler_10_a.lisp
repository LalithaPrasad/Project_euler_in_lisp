(defun is-prime (N)
  (cond
	((= N 2) T)
	((= N 3) T)
	((evenp N) NIL)
	(T (let ((d 3))
		 (loop
		   (if (zerop (rem N d)) (return NIL))
		   (incf d 2)
		   (when (> (* d d) N) (return T)))))))

(defun euler-10 (&optional (sum 2) (P 3))
  (cond
	((>= P 2000000) sum)
	((is-prime P) (euler-10 (+ sum P) (+ P 2)))
	(t (euler-10 sum (+ P 2)))))

(format t "~A~%" (euler-10))
