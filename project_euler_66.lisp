(defun make-sqrt-to-cf (d)
  (let* ((r (isqrt d)) (s r) (p 0) (q 1) (a 0))
	(lambda ()
	  (setf a s)
	  (setf p (- (* a q) p))
	  (setf q (/ (- d (* p p)) q))
	  (setf s (floor (+ r p) q)) a)))

(defun make-cnvgnts ()
  (let ((p-2 0) (q-2 1) (p-1 1) (q-1 0))
	(lambda (a-k)
	  (let ((p-k (+ (* a-k p-1) p-2))
			(q-k (+ (* a-k q-1) q-2)))
		(setf p-2 p-1 q-2 q-1 p-1 p-k q-1 q-k)
		(values p-k q-k)))))

(defun solve-pell-eqn (D)
  (let ((next-cf (make-sqrt-to-cf D))
		(next-cnvgnt (make-cnvgnts)))
	(loop do
		  (multiple-value-bind (p q) (funcall next-cnvgnt (funcall next-cf))
			(if (= 1 (- (* p p) (* D q q)))
				(return-from solve-pell-eqn p))))))

(defun not-square (D)
  (let ((r (isqrt D)))
	(/= D (* r r))))

(defun euler-66 ()
  (let ((maxD 0) (maxp 0))
	(loop for D from 2 to 1000 do
		  (if (not-square D)
			  (let ((p (solve-pell-eqn D)))
				(if (> p maxp) (setf maxD D maxp p)))))
	(values maxD maxp)))

(multiple-value-bind (maxD maxp) (euler-66)
  (format t "~d ~d~%" maxD maxp))
