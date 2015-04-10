;start weights for a network architecture (2 3 1) with bias values.
;you have a bias at the first and the second layer. This lead us to a
;structure like that (3 4 1).
;
; the following matrices we will get for the weights
;  
;    from layer 0 to layer 1 ==> 3x4
;    from layer 1 to layer 2 ==> 4x1
;
(defparameter xor-start-weights (make-array '(2))
(setf (aref xor-start-weights 0) (make-array '(3 3) :initial-contents 
	'(
		(-0.5855746971931446 0.9873503104348942 0.797965033186343) 
		(-0.42698549216339865 0.8973078389501696 0.727136484722597) 
		(-0.406504111035622 -0.5541410334195904 -0.7527936647237814)
	)))
(setf (aref xor-start-weights 1) (make-array '(4 1) :initial-contents 
	'(
		(0.05712822287606256)
		(0.450107593147832) 
		(-0.8002055023749701) 
		(0.8914689013244632)
	)))
