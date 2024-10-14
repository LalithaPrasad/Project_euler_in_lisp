(defun is-prime (N)
  (cond
	((< N 2) NIL)
	((= N 2) T)
	((= N 3) T)
	((evenp N) NIL)
	(T (let ((d 3))
		 (loop
		   (if (zerop (rem N d)) (return NIL))
		   (incf d 2)
		   (when (> (* d d) N) (return T)))))))

(defun split-num (n &optional (d 10))
  (let ((lst (cons n '())))
	(loop until (> d n) do
		  (setf lst (cons (floor n d) lst) lst (cons (rem n d) lst) d (* d 10)))
	lst))

(defun check-prime (n)
  (every #'is-prime (split-num n)))

(defun euler-37 ()
  (let ((n 9) (s 0) (count 0))
	(loop while (< count 11) do
		  (if (check-prime n)
			(setf s (+ s n) count (+ count 1) n (+ n 1))
			(setf n (+ n 1))))
	s))

(format t "~d~%" (euler-37))
