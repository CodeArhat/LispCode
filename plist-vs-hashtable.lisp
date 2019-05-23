(ql:quickload "alexandria")

(defun make-keyword (s)
  (alexandria:make-keyword (string-upcase s)))

(defun make-keys (n)
  (loop for i below n
        collect (make-keyword (format nil "~r" i))))

(defun make-plist (n)
  (apply #'append (loop for i below n
                        collect (list (make-keyword (format nil "~r" i)) i))))

(defun make-hashtable (n)
  (let ((ks (make-keys n))
        (pl (make-plist n))
        (ht (make-hash-table)))
    (dolist (k ks)
      (setf (gethash k ht)
            (getf pl k)))
    ht))

(defun plist-test ()
  )

(defun make-plist-array ())
