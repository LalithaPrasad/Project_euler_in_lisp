(defun euler-3 (n)
  (let ((p 2))
	(loop while (< p n) do (if (zerop (mod n p)) (setf n (/ n p)) (incf p)))
	p))

(format t "~A~%" (euler-3 600851475143))
