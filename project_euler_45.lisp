(defun euler-45 (c p q r)
  (labels ((triangular (n) (/ (* n (+ n 1)) 2))
		   (pentagonal (n) (/ (* n (- (* 3 n) 1)) 2))
		   (hexagonal (n) (* n (- (* 2 n) 1))))
	(let ((tr (triangular p)) (pe (pentagonal q)) (he (hexagonal r)))
	  (cond
		((= tr pe he) (if (= c 2) tr (euler-45 (+ c 1) p q (+ r 1))))
		((< pe he) (euler-45 c p (+ q 1) r))
		((< tr pe) (euler-45 c (+ p 1) q r))
		(t (euler-45 c p q (+ r 1)))))))


(format t "~A~%" (euler-45 1 2 2 2))
