(defun factorial (n)
  (if (= n 0)
	1
	(* n (factorial (- n 1)))))

(defun permute (perm lst fact ld)
  (cond
	((zerop perm) lst)
	(t (let ((d (nth (floor perm fact) lst)))
		 (cons d (permute (rem perm fact) (delete d lst) (floor fact ld) (- ld 1)))))))

(defun euler-24 (perm lst)
  (let ((n 0))
	(dolist (d (permute (1- perm) lst (factorial (1- (length lst))) (1- (length lst))))
	  (setf n (+ (* n 10) d)))
	n))

(format t "~a~%" (euler-24 1000000 '(0 1 2 3 4 5 6 7 8 9)))
