(defun len-num (n)
  (if (zerop n)
	0
	(+ 1 (len-num (floor n 10)))))

(defun num-to-list (n)
  (if (= n 0)
	nil
	(cons (rem n 10) (num-to-list (floor n 10)))))

(defun all-digits (n)
  (equal '(1 2 3 4 5 6 7 8 9) (sort (num-to-list n) #'<)))

(defun join-nums (n m)
  (read-from-string (concatenate 'string (write-to-string n) (write-to-string m))))

(defun euler-38 ()
  (let ((maxn 0))
	(loop for x from 1 below 10000 do
		  (let ((n x))
			(loop for y from 2 to 9 do
				  (setf n (join-nums n (* x y)))
				  (if (and (= (len-num n) 9) (all-digits n) (> n maxn))
					(setf maxn n)))))
	maxn))

(format t "~a~%" (euler-38))
