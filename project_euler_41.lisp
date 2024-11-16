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

(defun permutations (lst)
;; This function is taken from StackOverflow
  (cond
	((null lst) nil)
	((null (cdr lst)) (list lst))
	(t (loop for e in lst
			 append (mapcar (lambda (l) (cons e l)) (permutations (remove e lst)))))))

(defun convert (lst)
  (reduce (lambda (x y) (+ (* x 10) y)) lst))

(defun euler-41 ()
  (let ((lst '(9 8 7 6 5 4 3 2 1)))
	(loop while (consp lst)
		  maximize (loop for p in (permutations lst)
						 for n = (convert p)
						 when (is-prime n) maximize n)
		  do (pop lst))))

(format t "~a~%" (euler-41))
