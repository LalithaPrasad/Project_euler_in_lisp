(defun euler-4 ()
  (labels ((reverse-number (n)
						   (let ((r 0))
							 (loop while (> n 0) do
								   (setf r (+ (* r 10) (rem n 10)) n (floor n 10)))
							 r)))
	(let ((maxp 0))
	  (loop for p from 101 to 999 do
			(loop for q from p to 999 do
				  (let ((x (* p q)))
					(if (and (= x (reverse-number x)) (> x maxp))
					  (setf maxp x)))))
	  maxp)))

(format t "~A~%" (euler-4))
