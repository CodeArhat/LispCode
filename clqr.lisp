
(progn
  (defparameter *p* 1)
  (print *p*)
  (defparameter *p* 222)
  (print *p*)

  (defvar *v* 1)
  (print *v*)
  (defvar *v* 222)
  (print *v*))


(let ((a 1) (b 2))
  (multiple-value-setq (a b) (values 11 22))
  (format t "~s ~s~%" a b))
