(defparameter *nums* (make-array 28124 :initial-element t))

(defun euler-23 ()
  (labels ((divisor-sum (n) (1+ (apply #'+
									   (loop for d from 2 to (isqrt n)
											 when (zerop (rem n d)) collect d into left and collect (floor n d) into right
											 finally (return (delete-duplicates (append left right)))))))
		   (is-abundant (n) (> (divisor-sum n) n))
		   (abundant-list () (loop for n from 2 to 28123 when (is-abundant n) collect n))
		   (modify-nums ()
						(let ((nlst (abundant-list)))
						  (dolist (x nlst)
							(dolist (y nlst)
							  (let ((s (+ x y)))
								(if (and (< s 28124) (elt *nums* s)) (setf (elt *nums* s) nil))))))))
		   (let ((funcall (modify-nums)))
			 (loop for i from 0 to 28123 when (elt *nums* i) sum i))))

(format t "~a~%" (euler-23))
