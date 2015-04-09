(load "neural_network.lisp")

; example initialization
(defparameter *network*
  (make-instance 'neural-network :layer-neuron-count '(2 3 1)))

;; check initializatioon values
(print (layer-count *network*))
(print (layer-neuron-count *network*))
(print (weights *network*))
(print (activationf *network*))


;create a trainset
;(defparameter train (make-array '(2 '(2 1)) initial-element (random 1.0)))
; train the netwrok
; (trainf *network* train test)

;compute the outputs
(computef *network* (make-array '(2) :initial-contents '(0 0)))
(computef *network* (make-array '(2) :initial-contents '(1 0)))
(computef *network* (make-array '(2) :initial-contents '(0 1)))
(computef *network* (make-array '(2) :initial-contents '(1 1)))