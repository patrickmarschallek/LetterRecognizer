(defparameter xor-input (make-array (list 4) ))
(setf (aref xor-input 0 )  (make-array (list 1 2) :initial-contents '((0 0))))
(setf (aref xor-input 1 ) (make-array (list 1 2) :initial-contents '((0 1))))
(setf (aref xor-input 2 ) (make-array (list 1 2) :initial-contents '((1 0))))
(setf (aref xor-input 3 ) (make-array (list 1 2) :initial-contents '((1 1))))