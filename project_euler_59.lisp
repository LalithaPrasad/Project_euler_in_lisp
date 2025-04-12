(require :uiop)

(defun read-ciphers ()
  (with-open-file (stream "0059_cipher.txt" :direction :input)
	(loop for n in (uiop:split-string (read-line stream) :separator ",") 
		  collect (parse-integer n))))

(defun ascii-codes ()
  (loop for c across "abcdefghijklmnopqrstuvwxyz"
		collect (char-code c)))

(defun euler-59 ()
  (let* ((cipher-text (reverse (read-ciphers))) (codes (ascii-codes)) (ln (length cipher-text)))
	(loop for c1 in codes do
		  (loop for c2 in codes do
				(loop for c3 in codes do
					  (let ((result nil))
						(loop for i from 0 below ln by 3 do
							  (progn
								(push (logxor c3 (nth i cipher-text)) result)
								(push (logxor c2 (nth (+ i 1) cipher-text)) result)
								(push (logxor c1 (nth (+ i 2) cipher-text)) result)))
						(let ((txt (coerce (loop for c in result collect (code-char c)) 'string)))
						  (if (search " the " txt)
							(let ((key (coerce (list (code-char c1) (code-char c2) (code-char c3)) 'string))
								  (ascii-sum (loop for c across txt sum (char-code c))))
							  (return-from euler-59 (values key txt ascii-sum)))))))))))

(multiple-value-bind (v1 v2 v3) (euler-59)
  (format t "~S~%~S~%~S~%" v1 v2 v3))

