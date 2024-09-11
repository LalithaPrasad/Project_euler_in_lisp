(defun euler-25 ()
  (labels ((numdigits (n)
					  (do ((nd 0 (1+ nd))
						   (x n (floor x 10)))
						((zerop x) nd))))
	(do ((a 0 b)
		 (b 1 (+ a b))
		 (index 0 (1+ index)))
	  ((>= (numdigits a) 1000) index))))

(format t "~a~%" (euler-25))
