(defun primep (n)
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

(defun nprimes (a b &optional (n 0))
  (labels ((f (n a b) (+ (* n n) (* a n) b)))
	(if (primep (f n a b)) (nprimes a b (+ n 1)) n)))

(defconstant *limit* 1000)
(defun euler-27 ()
  (let ((nmax 0) (amax 0) (bmax 0))
	(loop for a from (- *limit*) to *limit* do
		  (loop for b from (- *limit*) to *limit* do
				(let ((np (nprimes a b)))
				  (if (> np nmax)
					(setf nmax np amax a bmax b)))))
	(values nmax amax bmax (* amax bmax))))

(multiple-value-bind (p q r s) (euler-27)
  (format t "~a ~a ~a ~a~%" p q r s))
