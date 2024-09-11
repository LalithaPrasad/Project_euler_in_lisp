(defun euler-16 (&optional (N (expt 2 1000)))
    (if (zerop N)
        0
        (+ (rem N 10) (euler-16 (floor N 10)))))

(format t "~A~%" (euler-16))