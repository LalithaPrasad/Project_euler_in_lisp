(defun is-prime (n)
  (labels ((helper (m k)
				   (let ((x (- (* 6 k) 1)) (y (+ (* 6 k) 1)))
					 (cond
					   ((> x m) t)
					   ((zerop (rem n x)) nil)
					   ((> y m) t)
					   ((zerop (rem n y)) nil)
					   (t (helper m (+ k 1)))))))

	(cond
	  ((< n 2) nil)
	  ((= n 2) t)
	  ((= n 3) t)
	  ((zerop (rem n 2)) nil)
	  ((zerop (rem n 3)) nil)
	  (t (helper (isqrt n) 1)))))

(defun gen-primes (nump)
  (labels ((is-prime (n primes)
					 (loop for x in primes do
						   (if (<= x (isqrt n))
							 (if (zerop (rem n x)) 
							   (return-from is-prime nil))
							 (return-from is-prime t)))))

	(let* ((primes '(2 3)) (cnt (length primes)) (n (+ (first (last primes)) 1)))
	  (loop while (< cnt nump) do
			(if (is-prime n primes)
			  (setf primes (append primes (list n)) cnt (+ cnt 1)))
			(incf n))
	  primes)))

(defun sum-list (lst)
  (if (null lst)
	0
	(+ (car lst) (sum-list (cdr lst)))))

(defun check-prime-sum (i m primes plength slimit)
  (if (> (+ i m) plength) 
	   (check-prime-sum 0 (- m 1) primes plength slimit)
	   (let ((list-sum (sum-list (subseq primes i (+ i m)))))
		 (cond
		   ((> list-sum slimit) (check-prime-sum 0 (- m 1) primes plength slimit))
		   ((not (is-prime list-sum)) (check-prime-sum (+ i 1) m primes plength slimit))
		   (t list-sum)))))

(defun euler-50 ()
  (let* ((plength 1000) (slimit 1000000) (primes (gen-primes plength)))
	(check-prime-sum 0 plength primes plength slimit)))

(format t "~A~%" (euler-50))
