(in-package cl-user)

(defpackage p2 (:use cl) (:export foo))
(defpackage p3 (:use cl) (:export foo))

(in-package p2)
(defparameter foo 2)

(in-package p3)
(defparameter foo 3)

(in-package cl-user)
(defparameter foo 1)
(format t "~a ~a ~a~%" foo p2:foo p3:foo)
