(defun euler-63 ()
  (loop for base from 1 to 10 sum
		(loop for pow from 1 to 30
			  count (= pow (length (format nil "~A" (expt base pow)))))))

(format t "~A~%" (euler-63))
