(ql:quickload "iterate")
(use-package :iterate)

(defun my-gcd (a b)
  (if (zerop a)
	b
	(my-gcd (rem b a) a)))

(defun euler-33 ()
  (let ((n1 0) (d1 0) (n2 0) (d2 0) (n 0) (d 0) (g 0) (a 1) (b 1) (p 0) (q 0))
	(iter outer (for x from 11 below 100)
		  (if (zerop (rem x 10))
			(next-iteration)
			(progn
			  (setf n1 (floor x 10) n2 (rem x 10))
			  (iter (for y from (+ x 1) below 100)
					(if (zerop (rem y 10))
					  (next-iteration)
					  (progn
						(setf d1 (floor y 10) d2 (rem y 10))
						(cond
						  ((= n1 d1) (setf n n2 d d2))
						  ((= n1 d2) (setf n n2 d d1))
						  ((= n2 d1) (setf n n1 d d2))
						  ((= n2 d2) (setf n n1 d d1))
						  (t (next-iteration)))
						(setf g (my-gcd n d) n (floor n g) d (floor d g))
						(setf g (my-gcd x y) p (floor x g) q (floor y g))
						(if (and (= n p) (= d q)) (setf a (* a x) b (* b y)))))))))
	(floor b (my-gcd a b))))

(format t "~d~%" (euler-33))
