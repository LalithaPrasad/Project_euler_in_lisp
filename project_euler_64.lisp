(defun sqrt-to-cf (d)
  ;;; I got this function by searching Google
  (let* ((r (isqrt d))
         (a r)
         (p 0)
         (q 1)
         (cf-terms (list r)))
    (when (= (* r r) d)
      (return-from sqrt-to-cf cf-terms))

    (loop
      (setf p (- (* a q) p))
      (setf q (/ (- d (* p p)) q))
      (setf a (floor (+ r p) q))
      (push a cf-terms)
      (when (= q 1)
        (return (nreverse cf-terms))))))

(format t "~D~%" (loop for n from 2 to 10000 count (oddp (1- (length (sqrt-to-cf n))))))
