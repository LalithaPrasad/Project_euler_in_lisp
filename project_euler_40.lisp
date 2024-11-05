(defun num-to-list (n)
  (labels ((helper (n)
				   (if (zerop n)
					 nil
					 (cons (rem n 10) (helper (floor n 10))))))
	(reverse (helper n))))

(defun euler-40 ()
  (let ((n 0) (d 0) (p 1))
	(loop while (<= d 1000000) do
		  (incf n)
		  (let ((nl (num-to-list n)))
			  (loop for x in nl do
					(incf d)
					(if (member d '(1 10 100 1000 10000 100000 1000000)) (setf p (* p x))))))
	p))

(format t "~A~%" (euler-40))
