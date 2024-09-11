(defun fact (n)
  (let ((f 1))
	(loop while (> n 1) do
		  (setf f (* f n) n (decf n)))
	f))

(defun euler-20 (n)
  (let ((s 0))
	(loop while (> n 0) do
		  (setf s (+ s (rem n 10)) n (floor n 10)))
	s))

(format t "~A~%" (euler-20 (fact 100)))
