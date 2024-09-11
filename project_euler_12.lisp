(defun next-trngl-num (&optional (n 0) (tnum 0))
  (lambda ()
	(incf n)
	(incf tnum n)
	tnum))

(defun count-factors (n)
  (loop for i from 1 to (sqrt n)
		when (zerop (rem n i)) collect i into left and collect (/ n i) into right
		finally (return (length (delete-duplicates (append left right))))))

(defun euler-12 ()
  (let ((tfun (next-trngl-num)) (cnt 0) (n 0))
	(loop
	  (setf n (funcall tfun))
	  (setf cnt (count-factors n))
	(when (> cnt 500) (return (values n cnt))))))

(multiple-value-bind (a b) (euler-12)
  (format t "~A ~A~%" a b))
