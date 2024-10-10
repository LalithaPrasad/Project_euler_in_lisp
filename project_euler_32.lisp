(defun permutations (lst)
;; This function is taken from StackOverflow
  (cond
	((null lst) nil)
	((null (cdr lst)) (list lst))
	(t (loop for e in lst
			 append (mapcar (lambda (l) (cons e l)) (permutations (remove e lst)))))))

(defun convert (lst)
  (reduce (lambda (x y) (+ (* x 10) y)) lst))

(defun euler-32 ()
  (let ((perms (permutations '(1 2 3 4 5 6 7 8 9))))
	(loop for p in perms
		  for a = (nth 0 p)
		  for b = (convert (subseq p 1 5))
		  for z1 = (convert (subseq p 5))
		  for x = (convert (subseq p 0 2))
		  for y = (convert (subseq p 2 5))
		  for z2 = (convert (subseq p 5))
		  when (= (* a b) z1) collect z1 into prod1
		  when (= (* x y) z2) collect z2 into prod2
		  finally (return (apply #'+ (remove-duplicates (nconc prod1 prod2)))))))
	

(format t "~d~%" (euler-32))
