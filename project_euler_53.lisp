(defun euler-53 ()
  (let ((factorials (make-hash-table)))
	(setf (gethash 0 factorials) 1)
	(setf (gethash 1 factorials) 1)
	(labels ((fact (n)
				   (or (gethash n factorials) (setf (gethash n factorials) (* n (fact (- n 1)))))))
	  (loop for n from 1 to 100
			sum (loop for r from 1 to n
					  and tmp = (/ (fact n) (fact r) (fact (- n r)))
					  when (> tmp 1000000) sum 1)))))

(format t "~A~%" (euler-53))
