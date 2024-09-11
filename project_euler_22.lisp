(ql:quickload :cl-utilities)

(defparameter *dict* (make-hash-table))

(defun euler-22 ()
  (labels ((form-dict () (loop for x from 1 to 26 do (setf (gethash (code-char (+ 64 x)) *dict*) x)))
		   (input-22 () (cl-utilities:split-sequence #\,
													 (car (with-open-file (stream "names.txt")
															(loop for line = (read-line stream nil)
																  while line collect line))))))
	(let ((names (sort (input-22) #'string-lessp)) (score 0) (count 0) (funcall (form-dict)))
	  (dolist (name names)
		(let ((tmp 0)) (incf count)
		  (loop for c across name do
				(multiple-value-bind (value present) (gethash c *dict*)
				  (if present (incf tmp value))))
		  (incf score (* count tmp))))
	  score)))

(format t "~a~%" (euler-22))
