(defmacro do-fib-list (end)
  `(let ((fibl '()))
	 (do ((var 1 next)
		  (next 1 (+ var next)))
		 ((> var ,end))
	   (push var fibl))
	 (reverse fibl)))

(format t "~D~%" (reduce #'+ (remove-if-not #'evenp (do-fib-list 4000000))))
