;;;; RSA加密算法测试

(in-package :ironclad)

(defclass rsa-signature ()
 ((s :initarg :s :reader rsa-signature-s)))

(defun make-rsa-signature (s)
 (make-instance 'rsa-signature :s s))

(defmethod sign-message ((key rsa-private-key) message &key (start 0) end)
 (make-rsa-signature (encrypt-message key (subseq message start end))))

(defmethod verify-signature ((key rsa-public-key)
                             message
                             (signature rsa-signature)
                             &key (start 0) end)
 (let ((nonzero (position-if #'plusp message :start start :end end)))
  (not (mismatch (subseq message nonzero end)
        (encrypt-message key (rsa-signature-s signature))))))

  (defun make-rsa-key-pair (bits &optional (e 65537) (prng *prng*))
   (flet ((find-prime (bits)
           (do ((x (generate-prime bits prng) (generate-prime bits prng))
                (attempts 10 (decf attempts)))
            ((= (egcd (1- x) e) 1) x)
            (when (zerop attempts) (error "Failed to produce a prime"))))) 
    (let* ((bits-p (floor (1+ bits) 2))
           (bits-q (- bits bits-p))
           (p (find-prime bits-p))
           (q (find-prime bits-q))
           (n (* p q))
           (d (modular-inverse e (* (1- p) (1- q)))))
     (values (make-private-key :rsa :n n :d d)
      (make-public-key :rsa :n n :e e)))))

(eval-when (:compile-toplevel :load-toplevel)
 (export '(make-rsa-key-pair rsa-signature-s)))
