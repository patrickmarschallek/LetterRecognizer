(setf words '(	(one . un)
				(two . deux)
				(three . trois)
				(four . quatre)
				(five . cinq)
				)
)

(defun translate (w)
	(cdr (assoc w words))
)

( defun myabs1 (x)
	(cond
		((< x 0) (- x))
		((>= x 0) x)
	)
)

(defun myabs2 (x)
	(cond
		((< x 0) (- x))
		(t x)
	)
)

(defun g (x)
	(cond 
		((< x 4) (- x 4))
		((<= x 5) (x))
		(T (+ x 3))
	)
)

(defun myMember (ele l)
	(cond 
		((null l) nil)
		((equal ele (car l)) l)
		( t (myMember ele (cdr l)))
	)
)

(defun h (x)
	(cond 
		((> x 0) 1)
		(t 0)
	)
)

(defun my_len (l)
	(cond
		(( null l) 0)
		( t (+ 1 (my_len(cdr l))))
	)
)

(defun my_emp (ele l)
	(cond
		((null l) nil)
		((= ele 0) (car l))
		((> ele 0) (my_emp(- ele 1) (cdr l)))
	)
)

(defun drop_minusValue (l)
	(cond
		((null l) nil)
		((minusp(car l)) (drop_minusValue(cdr l)))
		(t (cons (car l) (drop_minusValue(cdr l))))
	)
)

(defun my_append(l1 l2)
	(cond
		((null l1) l2)
		(t (cons (car l1) (my_append(cdr l1) l2)))
	)
)

(defun count_atom(l)
	(cond
		((null l) nil)
		( (atom l) 1)
		(t (+ (count(car l)) (count(cdr l))))
	)
)
		