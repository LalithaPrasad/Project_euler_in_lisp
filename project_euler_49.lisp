(defun all-permutations (lst &optional (remain lst))
  ;; Taken from Stack Overflow
  (cond
	((null lst) (list lst))
	((null remain) nil)
	((null (rest lst)) (list lst))
	(t (append
		 (mapcar (lambda (l) (cons (first lst) l))
				 (all-permutations (rest lst)))
		 (all-permutations
		   (append (rest lst) (list (first lst)))
		   (rest remain))))))


(defun is-prime (n)
  (labels ((helper (m k)
				   (let ((x (- (* 6 k) 1)) (y (+ (* 6 k) 1)))
					 (cond
					   ((> x m) t)
					   ((zerop (rem n x)) nil)
					   ((> y m) t)
					   ((zerop (rem n y)) nil)
					   (t (helper m (+ k 1)))))))

	(cond
	  ((< n 2) nil)
	  ((= n 2) t)
	  ((= n 3) t)
	  ((zerop (rem n 2)) nil)
	  ((zerop (rem n 3)) nil)
	  (t (helper (isqrt n) 1)))))

(defun num-to-list (n)
  (let ((l '()))
	(loop while (> n 0) do (setf l (cons (rem n 10) l) n (floor n 10)))
	l))

(defun list-to-num (lst)
  (let ((n 0))
	(loop for x in lst do (setf n (+ (* n 10) x)))
	n))

(defun permuted-num-list (n)
  (let ((nl '()))
	(loop for l in (all-permutations (num-to-list n)) do
		  (setf nl (cons (list-to-num l) nl)))
	nl))

(defun euler-49 ()
  (loop for n from 1001 to 9999 do
		(if (is-prime n)
		  (let ((n1 (+ n 3330)))
			(if (is-prime n1)
			  (let ((p (permuted-num-list n)))
				(if (member n1 p)
				  (let ((n2 (+ n1 3330)))
					(if (and (is-prime n2) (member n2 p))
					  (format t "~A~A~A~%" n n1 n2))))))))))

(euler-49)
