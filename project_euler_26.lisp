(defun find-cycle (d &optional (n 1) (digits '()))
  (let ((r (rem n d)))
	(cond
	  ((< n d) (find-cycle d (* n 10) digits))
	  ((member r digits) (length digits))
	  ((zerop r) 0)
	  (t (find-cycle d r (cons r digits))))))

(defun euler-26 ()
  (let ((maxc 0) (maxd 0))
	(loop for d from 2 below 1000 do
		  (let ((c (find-cycle d)))
			(if (> c maxc)
			  (setf maxc c maxd d))))
	(values maxc maxd)))

(multiple-value-bind (c d) (euler-26)
  (format t "~a ~a~%" c d))
