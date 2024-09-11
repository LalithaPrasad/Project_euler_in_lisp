(defun euler-2 (N)
	(labels ((helper (a b sum)
					 (cond
					   ((>= a N) sum)
					   ((evenp a) (helper b (+ a b) (+ sum a)))
					   (t (helper b (+ a b) sum)))))
	(helper 1 2 0)))

(format t "~d ~%" (euler-2 4000000))
