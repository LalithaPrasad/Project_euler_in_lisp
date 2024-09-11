(defun factorial (n)
  (if (= n 1)
	1
	(* n (factorial (- n 1)))))

(defvar *n* (factorial 40))
(defvar *m* (factorial 20))
(format t "~A~%" (floor (floor *n* *m*) *m*))
