(defparameter letter-desired (make-array (list 26)))
(setf (aref letter-desired 0 ) (make-array (list 26) :initial-contents '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 1 ) (make-array (list 26) :initial-contents '(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 2 ) (make-array (list 26) :initial-contents '(0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 3 ) (make-array (list 26) :initial-contents '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 4 ) (make-array (list 26) :initial-contents '(0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 5 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 6 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 7 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 8 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 9 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 10 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 11 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 12 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 13 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 14 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 15 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 16 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 17 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0)))
(setf (aref letter-desired 18 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0)))
(setf (aref letter-desired 19 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0)))
(setf (aref letter-desired 20 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0)))
(setf (aref letter-desired 21 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0)))
(setf (aref letter-desired 22 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0)))
(setf (aref letter-desired 23 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0)))
(setf (aref letter-desired 24 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)))
(setf (aref letter-desired 25 ) (make-array (list 26) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))