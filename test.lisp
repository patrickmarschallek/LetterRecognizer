(load "neural_network.lisp")
(load "input/letter_input.lisp")
(load "input/xor_input.lisp")
(load "desired/xor_desired.lisp")
(load "desired/letter_desired.lisp")

; example initialization
(defparameter *network*
  (make-instance 'neural-network :layer-neuron-count '(2 3 1)))

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
(setf (aref train 0) xor-input)
(setf (aref train 1) xor-desired)
(trainf *network* train 500 0.1)

;compute the outputs
(print (computef *network* (make-array '(1 2) :initial-contents '((0 0)))))
(print (computef *network* (make-array '(1 2) :initial-contents '((1 0)))))
(print (computef *network* (make-array '(1 2) :initial-contents '((0 1)))))
(print (computef *network* (make-array '(1 2) :initial-contents '((1 1)))))