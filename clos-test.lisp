(in-package :cl-user)
(defpackage :com.zlisp.clos-test
  (:use :cl))

(in-package :com.zlisp.clos-test)

(defgeneric dump (obj)
  )

(defmethod dump ((obj integer))
  (print obj))

(defmethod dump ((obj (eql nil)))
  (format t "false"))

(defmethod dump ((obj (eql t)))
  (format t "true"))

(defmethod dump ((obj (eql :null)))
  (format t "~a" "null"))
