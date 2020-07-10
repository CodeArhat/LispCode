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

(defclass Msg ()
  ())

(defclass InsertMsg (Msg)
  ())

(defclass UpdateMsg (Msg)
  ())

(defclass Encoder ()
  ())

(defclass Encoder1 (Encoder)
  ())

(defclass Encoder2 (Encoder)
  ())

(defgeneric encode (encoder msg)
  )

(defmethod encode ((encoeer Encoder1)
                   (msg InsertMsg))
  (format t "E1 ~~ InsertMsg~%"))

(defmethod encode ((encoeer Encoder2)
                   (msg InsertMsg))
  (format t "E2 ~~ InsertMsg~%"))

(defmethod encode ((encoeer Encoder1)
                  (msg UpdateMsg))
  (format t "E1 ~~ UpdateMsg~%"))

(defmethod encode ((encoeer Encoder2)
                   (msg UpdateMsg))
  (format t "E2 ~~ UpdateMsg~%"))
