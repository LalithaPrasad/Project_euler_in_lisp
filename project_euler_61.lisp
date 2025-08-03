;;;
;;; This was an exercise to translate Pyhthon code to Lisp
;;;
(defun isint (n)
	 (= n (floor n)))

(defun istriangular (n)
  (let ((u (sqrt (+ (* 8 n) 1))))
	(and (isint u) (isint (/ (- u 1) 2)))))

(defun issquare (n)
  (let ((u (sqrt n)))
	(isint u)))

(defun ispentagonal (n)
  (let ((u (sqrt (+ (* 24 n) 1))))
	(and (isint u) (isint (/ (+ u 1) 6)))))

(defun ishexagonal (n)
  (let ((u (sqrt (+ (* 8 n) 1))))
	(and (isint u) (isint (/ (+ u 1) 4)))))

(defun isheptagonal (n)
  (let ((u (sqrt (+ (* 40 n) 9))))
	(and (isint u) (isint (/ (+ u 3) 10)))))

(defun isoctagonal (n)
  (let ((u (sqrt (+ (* 12 n) 4))))
	(and (isint u) (isint (/ (+ u 2) 6)))))

(defun ispolygonal (n)
  (or
	(isoctagonal n)
	(isheptagonal n)
	(ishexagonal n)
	(ispentagonal n)
	(issquare n)
	(istriangular n)))

(defun check (vals funcs)
  (let ((temp '()))
	(loop for f in funcs do
		  (block f-loop
			(loop for v in vals do (block v-loop
									 (if (member v temp) (return-from v-loop))
									 (if (funcall f v)
										 (progn
										   (push v temp)
										   (return-from f-loop)))))))
	(= (length temp) 6)))

(defun euler-61 ()
  (let ((y1 nil) (y2 nil) (y3 nil) (y4 nil) (y5 nil) (y6 nil)
				 (funcs (list #'isoctagonal #'isheptagonal #'ishexagonal #'ispentagonal #'issquare #'istriangular)))
  (loop for x1 from 10 to 99 do
	(loop for x2 from 10 to 99
		do (block x2-loop
			(if (= x2 x1) (return-from x2-loop))
			(setf y1 (+ (* x1 100) x2))
			(if (not (ispolygonal y1)) (return-from x2-loop))
			(loop for x3 from 10 to 99
				do (block x3-loop
					(if (or (= x3 x1) (= x3 x2)) (return-from x3-loop))
					(setf y2 (+ (* x2 100) x3))
					(if (= y2 y1) (return-from x3-loop))
					(if (not (ispolygonal y2)) (return-from x3-loop))
					(loop for x4 from 10 to 99
						do (block x4-loop
							(if (or (= x4 x1) (= x4 x2) (= x4 x3)) (return-from x4-loop))
							(setf y3 (+ (* x3 100) x4))
							(if (or (= y3 y1) (= y3 y2)) (return-from x4-loop))
							(if (not (ispolygonal y3)) (return-from x4-loop))
							(loop for x5 from 10 to 99
								do (block x5-loop
									(if (or (= x5 x1) (= x5 x2) (= x5 x3) (= x5 x4)) (return-from x5-loop))
									(setf y4 (+ (* x4 100) x5))
									(if (or (= y4 y1) (= y4 y2) (= y4 y3)) (return-from x5-loop))
									(if (not (ispolygonal y4)) (return-from x5-loop))
									(loop for x6 from 10 to 99
										do (block x6-loop
											(if (or (= x6 x1) (= x6 x2) (= x6 x3) (= x6 x4) (= x6 x5)) (return-from x6-loop))
											(setf y5 (+ (* x5 100) x6))
											(if (or (= y5 y1) (= y5 y2) (= y5 y3) (= y5 y4)) (return-from x6-loop))
											(if (not (ispolygonal y5)) (return-from x6-loop))
											(setf y6 (+ (* x6 100) x1))
											(if (or (= y6 y1) (= y6 y2) (= y6 y3) (= y6 y4) (= y6 y5)) (return-from x6-loop))
											(if (not (ispolygonal y6)) (return-from x6-loop))
											(let ((vals (list y1 y2 y3 y4 y5 y6)))
											  (if (check vals funcs)
												(return-from euler-61 vals))))))))))))))))

(let ((vals (euler-61)))
  (format t "~S ~A~%" vals (reduce #'+ vals)))
