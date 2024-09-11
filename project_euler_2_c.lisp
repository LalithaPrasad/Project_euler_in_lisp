(defun euler-2 ()
  (let ((s 0))
	(do ((a 0 b) (b 1 (+ a b)))
	  ((> a 4000000) (return s))
	  (when (evenp a) (setf s (+ s a))))))

(format t "~A~%" (euler-2))
