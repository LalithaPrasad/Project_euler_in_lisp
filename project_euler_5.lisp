(defun euler-5 (n)
  (reduce #'(lambda (m n) (floor (* m n) (gcd m n))) (loop for i from 1 to 20 :collect i)))

(format t "~d~%" (euler-5 20))
