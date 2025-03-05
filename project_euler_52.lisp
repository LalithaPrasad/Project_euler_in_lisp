(defun num-to-list (n &optional (nl '()))
  (if (zerop n)
	nl
	(num-to-list (floor n 10) (push (rem n 10) nl))))

(defun all-digits-present (n num-list)
  (loop while (> n 0) do
		(if (not (member (rem n 10) num-list))
		  (return-from all-digits-present nil))
		(setf n (floor n 10)))
  (return-from all-digits-present t))

(defun test-multiples (n)
  (let ((num-list (num-to-list n)))
	(loop for x in '(2 3 4 5 6) do
		  (if (not (all-digits-present (* n x) num-list))
			(return-from test-multiples nil)))
	(return-from test-multiples t)))

(defun euler-52 ()
  (loop for n from 100000 below 1000000 do
		(if (test-multiples n)
		  (progn
			(format t "~A~%" n)
			(return)))))

(euler-52)

