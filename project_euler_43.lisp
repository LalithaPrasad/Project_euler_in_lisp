(defun permute (list)
; This function is from Rosetta code
  (if list
    (mapcan #'(lambda (x)
		(mapcar #'(lambda (y) (cons x y))
			(permute (remove x list))))
	    list)
    '(())))

(defun convert (lst)
  (reduce (lambda (x y) (+ (* x 10) y)) lst))

(defun divisible (lst)
  (every #'(lambda (x) (eq x T)) (loop for p in '(2 3 5 7 11 13 17)
				for i from 1 to 7
				collect (zerop (rem (convert (subseq lst i (+ i 3))) p)))))

(defun euler-43 ()
  (let ((perms (permute '(0 1 2 3 4 5 6 7 8 9))))
	(loop for p in perms
		when (divisible p) sum (convert p))))

(format t "~A~%" (euler-43))
