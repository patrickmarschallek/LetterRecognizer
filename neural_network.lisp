(load "matrix.lisp")

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
    :initform (lambda (x) (/ 1 (+ 1 (without-floating-point-underflow (exp (- x))))))
    :accessor activationf
    :documentation "the activation function of the neurons. Default is the sigmoid function.")
   (gradients
    :initform ()
    :accessor gradients
    :documentation "contains the computed outputs for each layer.")
   (weights
     :initform ()
     :accessor weights
     :documentation "initial startweights are random decimal numbers between 0 and 1.")))

;; initialization method
(defmethod initialize-instance :after ((network neural-network) &key initarg)
  (setf (slot-value network 'layer-count) (length (layer-neuron-count network)))
  (setf (slot-value network 'gradients) (let* (
      (layer-count (layer-count network))
      (layer-neuron-count (layer-neuron-count network))
      (gradient-count (decf layer-count))
      (g (make-array (list gradient-count)))
    )
    (dotimes (k gradient-count) 
      (setf i (+ k 1))
      (setf (aref g k) (make-array (list 1
          (nth (+ k 1) layer-neuron-count)
        ) :initial-element 0.0))
      (when (/= i gradient-count) 
        (setf (aref g i) (make-array (list 1
              (nth (+ i 1) layer-neuron-count)
            ) :initial-element 0.0))
        )
      )
   g ))
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
          (nth (+ k 1) layer-neuron-count)
        ) :initial-element (random 1.0)))
       (dotimes (n (nth k layer-neuron-count) )
        (dotimes (j (nth (+ k 1) layer-neuron-count))
            (setf (aref (aref w k) n j) (random 1.0))
          ))
      (when (/= i weight-count) 
        (progn 
        (setf (aref w i) (make-array (list
              (nth i layer-neuron-count) 
              (nth (+ i 1) layer-neuron-count)
            ))) 
        (dotimes (n   (nth i layer-neuron-count) )
          (dotimes (j (nth (+ i 1) layer-neuron-count))
              (setf (aref (aref w i) i j) (random 1.0))
            ))))
        )
   w )))

; this function computes the output for a specific 
; input the input must be two dimensional
(defun computef (network input)
    (setf weights (weights network))
    (setf weight-count (length weights))
    (dotimes (k weight-count) 
        (setf result (matrix-mul input (aref weights k)))
        (array-map result (activationf network))
        (setf (aref (gradients network) k ) result) 
        (when (/= (+ 1 k) weight-count)
          (setf input result)
          ))

      result
    )

;
; PSEUDO CODE from wikipedia 
;
;initialize network weights (often small random values)
; do
;    forEach training example ex
;       prediction = neural-net-output(network, ex)  // forward pass
;       actual = teacher-output(ex)
;       compute error (prediction - actual) at the output units
;       compute \Delta w_h for all weights from hidden layer to output layer  // backward pass
;       compute \Delta w_i for all weights from input layer to hidden layer   // backward pass continued
;       update network weights // input layer not modified by error estimate
; until all examples classified correctly or another stopping criterion satisfied
; return the network
;
(defun trainf (network train &optional (iterations 1000) (lernrate 0.1) (max-error 0.1))
    (setf training-set-count (nth 0 (array-dimensions train)))
    (dotimes (i iterations)
        (progn
          (setf net-error 0.0)
          (setf inputs (aref train 0))
          (setf desireds (aref train 1))
          (dotimes (k training-set-count)
            (progn
                (setf output (computef network (aref inputs k)))
                (setf error (compute-error output (aref desireds k)))
                (setf gradients (gradients network))
                (setf gradients-count (length gradients))
                (setf delta (make-array (list gradients-count)))
                (print error)
                (incf net-error error)
                (loop for m from gradients-count downto 1 do 
                  (progn 
                    (if (= gradients-count m)
                        (setf (aref delta (- m 1)) (output-delta network output (aref desireds k)))
                        (setf (aref delta (- m 1)) (hidden-delta network (- m 1) output (aref delta m)))
                      )))
                (setf weights (weights network))
                ;;update weights
            (dotimes (k gradients-count)
              (let* (
                  (row (nth 0 (array-dimensions (aref weights k))))
                  (column (nth 1 (array-dimensions (aref weights k))))
                )
                  (dotimes (i row)
                    (dotimes (j column)
                      (progn 
                        (decf (aref (aref weights k) i j) (* 
                          lernrate
                          (aref (aref delta k) 0)
                          (aref (aref gradients k) 0 j)
                        )))))))
            (setf (weights network) weights)
          )
          ; (print delta)
          (print (format nil "Iteration: ~,d" i))
          (print (format nil "Error: ~,5f" net-error))
          
        (when (< net-error max-error)
          (return))
        ))))

; calculates the mean squared error of all output neurons
(defun compute-error (output desired)
    (setf output-count (nth 0 (array-dimensions output)))
    (setf error 0.0)
    (dotimes (k output-count)
       (incf error (square (abs (- (aref output 0 k) (aref desired k)))))
      )
    error
  )
;
;  (o_j - t_j) sigmoid(net_j) (1-sigmoid(net_j) )
;
(defun output-delta (network o d)
    (setf function (activationf network))
    (setf length (nth 1 (array-dimensions o)))
    (setf result (make-array (list length)))
    (dotimes (i length) 
      (setf (aref result i) 
        (* 
          (- (aref o 0 i) (aref d i)) 
          (funcall function (aref o 0 i)) 
          (- 1 (funcall function (aref o 0 i)))))
      )
    result
  )
;
; (sum_l(delta w_j_l) ) sigmoid(net_j) (1-sigmoid(net_j) )
;
; function which calculates the difference for the hidden layer
(defun hidden-delta (network layer last-gradient last-delta)
    (setf function (activationf network))
    (setf length (nth 1 (array-dimensions last-gradient)))
    (setf result (make-array (list length)))
    (setf weights (aref (weights network) layer))
    (setf column (nth 1 (array-dimensions weights)))
    (dotimes (i length) 
      (setf delta-element 0.0)
      (dotimes (k column)
            (incf delta-element (* (aref weights i k) (aref last-delta i)))
      (setf (aref result i) (* 
        delta-element
        (funcall function (aref last-gradient 0 i)) 
        (- 1 (funcall function (aref last-gradient 0 i)))))
      ))   
    result
  )

(defun array-map (structure function) 
  (setf column (nth 0 (array-dimensions structure)))
  (setf row (nth 1 (array-dimensions structure)))
  (dotimes (i column) 
    (dotimes (j row) 
        (setf (aref structure i j) (funcall function (aref structure i j)))))
  structure
)

(defun square (x)
  (* x x))
