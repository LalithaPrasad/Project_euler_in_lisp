(defun reverse-num (n)
  (let ((r 0))
	(loop while (> n 0) do
		  (setf r (+ (* r 10) (rem n 10)) n (floor n 10)))
	r))

(defun bin-convert (n)
  (let ((bn 0) (m 1))
	(loop while (> n 0) do
		  (setf bn (+ bn (* m (rem n 2))) n (floor n 2) m (* m 10)))
	bn))

(defun euler-36 ()
  (loop for n from 1 below 1000000
		when (and (= n (reverse-num n)) (= (bin-convert n) (reverse-num (bin-convert n)))) sum n))


(format t "~d~%" (euler-36))
