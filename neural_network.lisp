(defclass neural-network ()
  ((layer-count
    :reader layer-count
    :documentation "keep the count of layers in this neural network.")
   (layer-neuron-count
    :initarg :layer-neuron-count
    :initform (error "it must be set a layer definition structure.")
    :accessor layer-neuron-count
    :documentation "list with neurons per layer")
   (activation
    :initarg activation 
    :initform (lambda (x) (/ 1 (+ 1 (exp (- x)))))
    :accessor activationf
    :documentation "the activation function of the neurons. Default is the sigmoid function.")
   (layer-bias
    :initform ()
    :accessor layer-bias
    :documentation "not used yet")
   (weights
     :initform ()
     :accessor weights
     :documentation "initial startweights are random decimal numbers between 0 and 1.")))

;; initialization method
(defmethod initialize-instance :after ((network neural-network) &key initarg)
  (setf (slot-value network 'layer-count) (length (layer-neuron-count network)))
  (setf (slot-value network 'layer-bias) ())
  (setf (slot-value network 'weights) (let* (
      (layer-count (layer-count network))
      (layer-neuron-count (layer-neuron-count network))
      (weight-count (decf layer-count))
      (w (make-array (list weight-count)))
    )
    (dotimes (k weight-count) 
      (setf i (+ k 1))
      (setf (aref w k) (make-array (list
          (nth k layer-neuron-count) 
          (nth (incf k) layer-neuron-count)
        ) :initial-element (random 1.0)))
      (when (/= i weight-count) 
        (setf (aref w i) (make-array (list
            (nth i layer-neuron-count) 
            (nth (incf i) layer-neuron-count)
          ) :initial-element (random 1.0)))
        )
      )
   w )))

(defun computef (network input)
    ;calculate output vector
    (print "not implemented yet.")
  )

(defun trainf (network train)
   ;train the neural network
    (print "not implemented yet.")
  )

(defun list-dimensions (list depth)
  (loop repeat depth
        collect (length list)
        do (setf list (car list))))

(defun list-to-array (list depth)
  (make-array (list-dimensions list depth)
              :initial-contents list))