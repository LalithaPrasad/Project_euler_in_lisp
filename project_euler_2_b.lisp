(defun euler-2 ()
  (let ((a 0) (b 1) (tmp 0) (s 0))
	(loop
	  (setf s (if (evenp a) (+ s a) s) tmp a a b b (+ tmp b))
	  (when (> a 4000000) (return s)))))

(format t "~A~%" (euler-2))
