(in-package :cl-user)

(defpackage my-test
  (:use
   :cl
   :prove))

(in-package :my-test)

(subtest "Showing off Prove"
  (ok (not (find 4 '(1 2 4 3))))
  (is t (not (find 4 '(1 2 4 3))))
  (is 4 4)
  (isnt 1 #\1))

(finalize)
