(load "neural_network.lisp")
(load "input/letter_input.lisp")
(load "input/xor_input.lisp")
(load "desired/xor_desired.lisp")
(load "desired/letter_desired.lisp")

; example initialization
(defparameter *network*
  (make-instance 'neural-network :layer-neuron-count '(35 13 26)))

;; check initializatioon values
(print (layer-count *network*))
(print (layer-neuron-count *network*))
(print (weights *network*))
(print (gradients *network*))
(print (activationf *network*))


;create a trainset
;(defparameter train (make-array '(2 '(2 1)) initial-element (random 1.0)))
; train the netwrok
(setf train (make-array '(2)))
(setf (aref train 0) letter-input)
(setf (aref train 1) letter-desired)
(trainf *network* train 1 0.1)

;compute the outputs
; (loop for i from 0 to 25 do
; 	(print (computef *network* (aref letter-input i)))
	; )