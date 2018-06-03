(defpackage :com.zlisp.wz
  (:use :cl)
  (:export :version))

(in-package :com.zlisp.wz)

(ql:quickload "hu.dwim.perec.oracle")

(defun version ()
  (format t "0.1"))
