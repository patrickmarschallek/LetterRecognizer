(defun matrix-mul (a b)
(let* (
		(m (nth 0 (array-dimension a)))
		(s (nth 1 (array-dimension a)))
		(n (nth 1 (array-dimension b)))
		(result  (make-array (list m n)))
	)
(dotimes i m (dotimes j n)
	(setf (aref result i j ) 0.0)
	(dotimes (k s)
		(incf (aref result i j) (* (aref a i j) (aref b k j))))
)))