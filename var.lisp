(defparameter sv 1)

(let ((lv 2))
  (defun foo ()
    (format t "sv=~a lv=~a~%" sv lv))

  (defun bar ()
    (foo)
    (let ((lv 3)
          (sv 4))
      (foo))
    (foo)))

(defun tctest (n &optional (m 0))
  ;(declare (optimize (speed 3) (safety 0) (debug 0)))
  ;(declare (fixnum n m))
  (declare (optimize (debug 3) (safety 3)))
  (if (= n 0)
      m
      (tctest (- n 1) (+ m 1))))

(defun rctest (n)
  (if (= n 0)
      0
      (+ (rctest (- n 1)) 1)))
