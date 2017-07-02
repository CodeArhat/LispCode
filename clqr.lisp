(defun demo (title cases)
  (format t "~&~a~%" title)
  (dolist (e cases)
    (format t "~&(~s~{~^ ~s~}): ~s~%" (car e) (cdr e) (apply (symbol-function (car e)) (cdr e)))))

(defun demo2 (title funcs args)
  (let ((lns (mapcar (lambda (x) (length (format nil "~s" x))) args)))
    (format t "~&~va: " (length title) title)
    (dotimes (i (length args))
      (format t "~vs " (nth i lns) (nth i args)))
    (dolist (fn funcs)
      (format t "~&~va: " (length title) fn)
      (dotimes (i (length args))
        (format t "~vs " (nth i lns) (apply (symbol-function fn) (nth i args)))))))


(defun dump-symbols (syms)
  (dolist (s syms)
    (format t "~&~a: ~s~%" s (symbol-value s))))

;;; 1 Numbers

;; 1.1 Predicates
(demo2 "op" '(= /= < >= > <=)
       '((1) (1 1) (1 2) (2 1) (1 2 3) (3 2 1) (1 1 2) (2 2 1) (1 2 1)))

(print (list (minusp -1) (minusp -2/3) (minusp -2.3)
             (minusp 0)))
;; plusp ~ minusp

(print (list (zerop 0) (zerop 0/3) (zerop 0.0) (zerop 0.0d0) (zerop #C(0 0))
             (zerop least-positive-double-float)))


(print (mapcar #'evenp '(0 -2 1)))
;; oddp ~ evenp

(let ((objs '(nil 0 1/2 0.0 0d0 0s0 #C(0 0) #c(0 1))))
  (dolist (fn (list #'identity #'numberp #'realp #'rationalp #'floatp #'integerp #'complexp))
    (format t "~&~30s: " fn)
    (dolist (x objs)
      (format t "~5s " (funcall fn x)))))

(print (random-state-p *random-state*))

;; 1.2 Numeric Functions
(print (list (+) (+ 2) (+ 1 2 3 4)))
(print (list (*) (* 2) (* 1 2 3 4)))

(print (list (- 2) (- 1 2 3 4)))
(print (list (/ 2) (/ 1 2 3 4)))

(let ((a 0))
  (print a)
  (print (1+ a))
  (print a)
  (print (incf a))
  (print a)
  (print (1- a))
  (print a)
  (print (decf a))
  (print a)
  (print (incf a 1/3))
  (print (decf a -0.66)))

(demo "math-functions"
      `((exp 0) (exp 1) (exp #c(0 3.1415926))
                (expt 2 8) (expt 4 1/2) (expt 9.0 0.5d0)
                (log 2.7182817) (log 8 2)
                (sqrt 5)
                (isqrt 5)
                (lcm) (lcm 2 3 4)
                (gcd) (gcd 4 6) (gcd 15 6 30)
                (sin ,(/ pi 2))
                (cos 0)
                (tan ,(/ pi 4))
                (asin 1)
                (acos 0)
                (atan 1) (atan 1 ,(sqrt 3))
                (sinh 0) (cosh 0) (tanh 0)
                (asinh 0) (acosh 1) (atanh 0)
                (cis 3.1415926)
                (conjugate #c(2 3))
                (max 1) (max 0 2 -1)
                (min 2) (min 0 2 -1)
                (mod 2 5) (rem 2 5)
                (mod 2 -5) (rem 2 -5)
                (random 5) (random 5)
                (random 5.0) (random 5.0)
                (random 5.0d0) (random 5.0d0)
                (float-sign 1.0 2.0) (float-sign -2.0 3.0)
                ))

(print (list (random 10000 (make-random-state *random-state*))
             (random 10000 (make-random-state *random-state*))
             (random 10000)
             (random 10000)))

;; 1.3 Logic Functions

;; 1.4 Integer Functions

;; 1.5 Implementation-Dependent
(dump-symbols
 '(short-float-epsilon single-float-epsilon double-float-epsilon long-float-epsilon

   short-float-negative-epsilon single-float-negative-epsilon
   double-float-negative-epsilon long-float-negative-epsilon

   most-positive-short-float least-positive-short-float least-positive-normalized-short-float
   most-positive-single-float least-positive-single-float least-positive-normalized-single-float
   most-positive-double-float least-positive-double-float least-positive-normalized-double-float
   most-positive-long-float least-positive-long-float least-positive-normalized-long-float

   most-negative-short-float least-negative-short-float least-negative-normalized-short-float
   most-negative-single-float least-negative-single-float least-negative-normalized-single-float
   most-negative-double-float least-negative-double-float least-negative-normalized-double-float
   most-negative-long-float least-negative-long-float least-negative-normalized-long-float
   ))

;; 9 Control Structure

;; 9.1 Predicates
(demo2 "eqs   " '(eq eql equal equalp)
       `((a a) ((a) (a)) (,(list 'a) ,(list 'a)) (#\a #\a) (#\a #\A) ("a" "a") ("a" "A") (1 1) (1 1.0)))

;; not === null

(demo "control predicates"
      `((boundp pi)
        (boundp ,(gensym))
        (constantp internal-time-units-per-second)
        (functionp ,#'sin)
        (fboundp sin)
        (fboundp defun)
        ))

;; (function 'foo) === #'foo

;; (values 1 2 3) === (values-list '(1 2 3))

;; lst === (multiple-value-list (values-list lst))

(nth-value 2 (values 0 1 2))

;; oddp === (complement #'evenp)

(flet ((foo (x) (* x x))) (fboundp 'foo)) ; fboundp check global function/macro only

(flet ((foo (x) (* x 2))) (function-lambda-expression 'foo))

(dump-symbols '(call-arguments-limit lambda-parameters-limit multiple-values-limit))

;; 9.2 Variables
(progn
  (defconstant +test-const+ 1)
  (defconstant +test-const+ 1) ;ok, same value
  (defconstant +test-const+ 2)) ;error

(progn
  (defparameter *p* 1)
  (print *p*)
  (defparameter *p* 222)
  (print *p*))

(progn
  (defvar *v* 1)
  (print *v*)
  (defvar *v* 222) ;no effect
  (print *v*))

(let ((a) (b))
  (setf a 1
        b (* a 2)))

(let ((a) (b))
  (psetf a 1
         b (* a 2))) ;error: parallel set, a is NIL

(let ((a)
      (b (make-array 3)))
  (setq a 1)
  (set 'a 2)
  (setq (aref b 0) 1)) ;error: symbol only

(let ((a 1) (b 2))
  (multiple-value-setq (a b) (values 11 22))
  (format t "~s ~s~%" a b))

(let ((a 1) (b 2) (c 3))
  (shiftf a b c)
  (format t "~&~s ~s ~s~%" a b c)

  (setf a 1 b 2 c 3)
  (rotatef a b c)
  (format t "~&~s ~s ~s~%" a b c))

(progn
  (defparameter *foo* 123)
  (makunbound '*foo*)
  (makunbound '*foo*) ;ok
  (print *foo*))

(progn
  (print (get 'foo 'bar))
  (setf (get 'foo 'bar) 123)
  (print (get 'foo 'bar)))

;; 9.3 Functions
(defun foo ()
  (flet ((foo () 2))
    (print (foo))
    (funcall (function foo)))
  1)

(list
  (apply (function sin) '(1))
  (apply 'sin '(1))
  (apply #'sin '(1))
  )

;; 9.7 Loop Facility
(loop (print (eval (read))))

;; 12 Types and Classes

(demo "type-functions"
      `((typep 1 integer)
        (typep 1 rational)
        (typep 1 number)
        (typep 1 atom)
        (typep 1 t)
        (typep 1 float)
        (typep 1 complex)
        (typep 1 cons)
        (typep 1 list)
        (typep 1 function)
        (type-of ,pi)
        ))

;; 13 Input/Output


;; 13.1 Predicates

;; 13.3 Character Syntax

;; 13.4 Printer

;; 13.5 Format
(format t (formatter "~&#~8,3f#~%") pi)
(print (format nil "~5,3f" pi)) ; output to string

(let ((f 1.2)
      (e 1.2e8))
  (format t "~&~&&~&")
  (format t "~%~%%~%")
  (format t "|~a|~s|~5a|~5s|~5@a|~5@s|~%" "ok" "ok" "ok" "ok" "ok" "ok")
  (format t "|~3,'0d|~%" 1)
  (format t "~a ~s ~f~%" pi pi pi)
  (format t "~2r~%" 15)
  (format t "~r ~:r ~@r ~@:r~%" 17 17 17 17)
  (format t "|~a|~s|~d|~b|~o|~x|~2,'0x|~%" 15 15 15 15 15 15 15)
  (format t "|~f|~@f|~8,3f|~%" f f f)
  (format t "|~f|~e|~g|~@e|~@g|~%" f f f f f)
  (format t "|~f|~e|~g|~@e|~@g|~%" e e e e e)
  (format t "|~c|~:c|~@c|~@:c|~%" #\a #\a #\a #\a)
  (format t "|~(~a~)|~:(~a~)|~@(~a~)|~@:(~a~)|~%" "oKi ha" "oKi ha" "oKi ha" "oKi ha")
  (format t "~&|~5a|~%~:*|~5s|" "ok")
  (format t "~?" "~s|~f|~%" '("ok" 3.14))
  (format t "|~vd|~%" 9 123))

;; 13.6 Streams


;; 13.7 Pathnames and Files


;; 14 Packages and Symbols


;; 14.1 Predicates


;; 14.2 Packages
(dolist (pkg (list-all-packages))
  (format t "~a: ~a~%" (package-name pkg)
          (package-nicknames pkg))) ;; return a list

(format t "~&~s ~s~%" (find-package "CL") (find-package "cl"))

(format t "~&~{~s~%~}" (find-all-symbols 'foo))

;; 14.3 Symbols
(make-symbol "foo")
(make-symbol "fOo")
(print (eq nil '()))

;; 14.4 Standard Packages


;; 15 Compiler


;; 15.1 Predicates
(dolist (fn '(block catch eval-when flet function go if labels let let* load-time-value locally macrolet multiple-value-call multiple-value-prog1 progn progv quote return-from setq symbol-macrolet tagbody the throw unwind-protect))
  (format t "~s: ~s~%" fn (special-operator-p fn)))

(compiled-function-p #'sin)

;; 15.2 Compilation
(quote (foo bar))
(eval '(print 123))

;; 15.3 REPL and Debugging

(time (sleep 0.5))
(room)
(gc)
(room)

(print -)

;; 15.4 Declarations

;; globally
(proclaim (optimize (speed 3)))
(proclaim (optimize (space 0)))

(declaim (optimize (speed 3)
                   (space 0)
                   (debug 0)
                   (safety 0)
                   (compilation-speed 0)))

(defun foo ()
  ;; locally
  (declare (optimize (speed 3)
                     (space 0)
                     (debug 0)
                     (safety 0)
                     (compilation-speed 0)))
  )

;; 16 External Environment
(demo "External Environment"
      `((get-internal-real-time)
        (get-internal-run-time)
        (short-site-name)
        (long-site-name)
        (lisp-implementation-type)
        (lisp-implementation-version)
        (software-type)
        (software-version)
        (machine-type)
        (machine-version)
        (machine-instance)))

(progn
  (print (get-internal-real-time))
  (print (get-internal-run-time))
  (sleep 1)
  (print (get-internal-real-time))
  (print (get-internal-run-time)))

(let* ((utm (get-universal-time)))
  (format t "~&internal-time-units-per-second: ~s~%" internal-time-units-per-second)
  (format t "~&universal-time: ~s~%" utm)
  (format t "decoded: ~s~%" (multiple-value-list (decode-universal-time utm)))
  (format t "(get-decoded-time):~{~^ ~s~}~%" (multiple-value-list (get-decoded-time)))
  (multiple-value-bind (sec min hour date mon year day daylightp tmzone) (decode-universal-time utm)
    (format t "encoded: ~s~%" (encode-universal-time sec min hour date mon year tmzone))
    (format t "weekday: ~s daylight: ~s~%" day daylightp)))

