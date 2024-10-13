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

(defun len (n)
  (if (zerop n)
	0
	(+ 1 (len (floor n 10)))))

(defun rotate (n)
  (let* ((l (len n)) (p (expt 10 (- l 1))) (lst '()))
	(loop for nil from 0 below l do
		  (setf n (+ (* p (rem n 10)) (floor n 10)) lst (cons n lst)))
	lst))

(defun circular-prime (n)
  (every #'is-prime (rotate n)))

(defun euler-35 ()
  (let ((cprimes '(2 3 5 7 11 13 17 31 37 71 73 79 97)))
	(loop for n from 101 below 1000000 do
		  (when (circular-prime n) (setf cprimes (append cprimes (rotate n)))))
	(length (remove-duplicates cprimes))))

(format t "~d~%" (euler-35))
