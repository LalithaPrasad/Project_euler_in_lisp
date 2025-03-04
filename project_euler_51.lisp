(defun is-prime (N)
  (cond
	((= N 2) T)
	((= N 3) T)
	((evenp N) NIL)
	(T (let ((d 3))
		 (loop
		   (if (zerop (rem N d)) (return NIL))
		   (incf d 2)
		   (when (> (* d d) N) (return T)))))))

(defun repeating-digits (n &optional (ud '()) (rd '()))
  (if (zerop n)
	rd
	(let ((r (rem n 10)))
	  (if (member r ud)
		(if (member r rd)
		  (repeating-digits (floor n 10) ud rd)
		  (repeating-digits (floor n 10) ud (append (list r) rd)))
		(repeating-digits (floor n 10) (append (list r) ud) rd)))))

(defun num-to-list (n &optional (nl '()))
  (if (zerop n)
	nl
	(num-to-list (floor n 10) (push (rem n 10) nl))))

(defun list-to-num (nl &optional (n 0))
  (if (null nl)
	n
	(list-to-num (rest nl) (+ (* n 10) (first nl)))))

(defun num-length (n)
  (if (zerop n)
	0
	(+ 1 (num-length (floor n 10)))))

(defun list-of-primes (llimit rlimit)
  (loop for n from llimit below rlimit
		when (and (is-prime n) (not (null (repeating-digits n))))
		collect n))

(defun euler-51 (l r)
  (let ((pl (list-of-primes l r)))
	(loop for p in pl do
		  (let ((rd (repeating-digits p)) (lp (num-length p)))
			(loop for d in rd do
				  (let ((lst '()))
					(loop for x in '(0 1 2 3 4 5 6 7 8 9) do
						  (let ((n (list-to-num (substitute x d (num-to-list p)))))
							(if (and (= (num-length n) lp) (is-prime n) (not (member n lst)))
							  (progn
								(push n lst)
								(delete n pl)))))
					(if (> (length lst) 7) (format t "~S~%" (sort lst #'<)))))))))

(euler-51 100000 1000000)

