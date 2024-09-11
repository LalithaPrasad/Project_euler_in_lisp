(defun euler-25 ()
  (do ((a 0 b)
	   (b 1 (+ a b))
	   (index 0 (1+ index)))
	((>= (length (write-to-string a)) 1000) index)))

(format t "~a~%" (euler-25))
