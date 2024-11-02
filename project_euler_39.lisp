(defun euler-39 ()
  (let* ((limit 1000) (arr (make-array (+ limit 1) :initial-element 0)))
	(labels ((max-index (maxp maxv p)
						(cond
						  ((>= p limit) (values maxp maxv))
						  ((> (aref arr p) maxv) (max-index p (aref arr p) (+ p 1)))
						  (t (max-index maxp maxv (+ p 1))))))
	  (loop for x from 1 to limit do
			(loop for y from x to limit do
				  (loop for z from y to limit do
						(let ((p (+ x y z)) (q (+ (* x x) (* y y))) (r (* z z)))
						  (if (and (<= p limit) (= q r))
							(let ((s (aref arr p)))
							  (setf (aref arr p) (+ s 1))))))))
	  (max-index 0 0 0))))

(multiple-value-bind (p v) (euler-39)
  (format t "~a ~a~%" p v))
