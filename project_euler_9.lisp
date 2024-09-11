(defun euler-9 ()
  (loop for a from 1 to 1000 by 1 do
		(loop for b from (+ a 1) to 1000 by 1 do
			  (let ((c (- 1000 a b)))
				(if (and (< a b c) (= (+ (* a a) (* b b)) (* c c)))
				  (return-from euler-9 (values a b c (* a b c))))))))

(multiple-value-bind (a b c d) (euler-9) (format t "~A ~A ~A ~A~%" a b c d))
