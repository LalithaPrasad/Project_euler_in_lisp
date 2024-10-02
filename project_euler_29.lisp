(defun euler-29 ()
  (length (delete-duplicates (loop for a from 2 to 100 append (loop for b from 2 to 100 collect (expt a b))))))

(format t "~d~%" (euler-29))
