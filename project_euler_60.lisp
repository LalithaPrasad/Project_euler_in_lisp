(defun is-prime (n)
  (cond
	((< n 2) NIL)
	((= n 2) T)
	((= n 3) T)
	((evenp n) NIL)
	(T (let ((d 3))
		 (loop
		   (if (zerop (rem n d))
			 (return NIL))
		   (incf d 2)
		   (when (> (* d d) n) (return T)))))))

(defun make-next-prime (&optional (m 1))
	(lambda ()
	  (loop while (not (is-prime (incf m))))
	  m))

(defun num-to-list (n &optional (lst nil))
  (if (zerop n)
	lst
	(num-to-list (floor n 10) (push (rem n 10) lst))))

(defun list-to-num (lst &optional (r 0))
  (if (null lst)
	r
	(list-to-num (rest lst) (+ (* r 10) (first lst)))))

(defun chk-prim (n1 n2)
  (let ((l1 (num-to-list n1)) (l2 (num-to-list n2)))
	(let ((m1 (list-to-num (concatenate 'list l1 l2))) (m2 (list-to-num (concatenate 'list l2 l1))))
	  (if (and (is-prime m1) (is-prime m2))
		t
		nil))))

(defun make-primes (n)
  (let ((next-prime (make-next-prime)) (primes '()))
	(dotimes (_ n) (push (funcall next-prime) primes))
	(reverse primes)))

(defun euler-60 ()
  (let* ((n 1500) (plist (make-primes n)))
	(loop for p1 in plist do
		  (loop for p2 in plist do
				(if (chk-prim p1 p2)
				  (loop for p3 in plist do
						(if (and (chk-prim p1 p3) (chk-prim p2 p3))
						  (loop for p4 in plist do
								(if (and (chk-prim p1 p4) (chk-prim p2 p4) (chk-prim p3 p4))
								  (loop for p5 in plist do
										(if (and (chk-prim p1 p5) (chk-prim p2 p5) (chk-prim p3 p5) (chk-prim p4 p5))
										  (return-from euler-60 (values p1 p2 p3 p4 p5 (+ p1 p2 p3 p4 p5))))))))))))))

(multiple-value-bind (p1 p2 p3 p4 p5 s) (euler-60)
  (format t "~A ~A ~A ~A ~A ~A~%" p1 p2 p3 p4 p5 s))
