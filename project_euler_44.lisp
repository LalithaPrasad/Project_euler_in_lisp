(defun euler-44 ()
  (labels ((pentagonal (x)
					   (/ (* x (- (* 3 x) 1)) 2))

		   (is-pentagonal (p)
						  (let ((x (/ (+ (sqrt (+ (* 24 p) 1)) 1) 6)))
							(if (= x (floor x))
							  t
							  nil)))

		   (helper (found n m)
				   (cond
					 (found (values (pentagonal m) (pentagonal n)))
					 ((< n 1) (helper nil m (+ m 1)))
					 (t (let ((p (pentagonal m)) (q (pentagonal n)))
						  (if (and (is-pentagonal (+ p q)) (is-pentagonal (- p q)))
							(helper t n m)
							(helper nil (- n 1) m)))))))
	(helper nil 2 1)))

(multiple-value-bind (p q) (euler-44)
	(format t "~A  ~A  ~A  ~A~%" p q (+ p q) (- p q)))
