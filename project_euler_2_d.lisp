(defun euler-2 ()
  (loop for x = 0 then y
		and y = 1 then (+ x y)
		while (<= x 4000000)
		when (evenp x) sum x))

(format t "~A~%" (euler-2))
