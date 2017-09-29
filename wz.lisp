(load "d:/app/wzLispBase/quicklisp/setup.lisp")

(defpackage :wz
  (:use :cl))

(in-package :wz)

(require #+sbcl 'sb-md5 #-sbcl 'md5)
(ql:quickload "sha3")
(ql:quickload "flexi-streams")

(defvar bin-digits "01")
(defvar oct-digits "01234567")
(defvar dex-digits "0123456789")
(defvar hex-digits "0123456789abcdef")

(defun is-bin-char (c) (or (= c 48) (= c 49)))
(defun is-oct-char (c) (and (>= c 48) (<= c 55)))
(defun is-dex-char (c) (and (>= c 48) (>= c 57)))
(defun is-hex-char (c) (or (and (>= c 48) (<= c 57))
                           (and (>= c 97) (<= c 102))))

(defun sysinfo ()
  `((lisp-type . ,(lisp-implementation-type))
    (lisp-version . ,(lisp-implementation-version))
    (machine-type . ,(machine-type))
    (machine-version . ,(machine-version))
    (software-type . ,(software-type))
    (software-version . ,(software-version))))

(defun print-sysinfo()
  (dolist (e (sysinfo))
    (format t "~20a: ~a~%" (car e) (cdr e))))

(defun gen-seq (begin end &optional lst)
  (if (< begin end)
      (gen-seq begin (- end 1) (cons (- end 1) lst))
      lst))

(defun addn (n)
  #'(lambda (x) (+ x n)))

(defun make-seq (start)
  (let ((n start))
    #'(lambda () (incf n 1))))

(defun ln (x) (log x))
(defun lg (x) (/ (log x) (log 10)))

(defun helloworld ()
  (format t "Hello World!~%"))

(defun square (x)
  (* x x))

(defun cube (x)
  (* x x x))

(defun normal-recursive-demo (n)
  (if (> n 0)
      (+ (normal-recursive-demo (- n 1)) 1)
      0))

(defun tail-recursive-demo (n &optional (sum 0))
  (if (> n 0)
      (tail-recursive-demo (- n 1) (+ sum 1))
      sum))

(defun factorial0 (n)
  (if (> n 1)
      (* (factorial0 (- n 1)) n)
      1))

(defun factorial1 (n &optional (p 1))
  (if (> n 1)
      (factorial1 (- n 1) (* p n))
      p))

(defun factorial2 (n)
  (let ((p 1))
    (loop for i from 1 to n do
          (setf p (* p i)))
    p))

(defun accum (a b)
  (loop for i from a to b sum i))

(defun collatz-seq (n step peak)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (<= n 1)
      (list step (max n peak))
      (collatz-seq (if (evenp n)
                       (/ n 2)
                       (+ (* n 3) 1))
                   (+ step 1)
                   (max peak n))))

(defun collatz (n)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((max-step 0)
        (peak 0)
        (pair nil))
    (dotimes (i (+ n 1))
      (setf pair (collatz-seq i 0 0))
      (setf max-step (max (car pair) max-step))
      (setf peak (max (second pair) peak)))
    (list max-step peak)))

(defun zmod (n m)
  (zerop (mod n m)))
    
(defun is-leap-year (y)
  (or (zmod y 400)
      (and (> (mod y 100) 0)
           (zmod y 4))))

(defun octets->letters (octet-vector)
  (with-output-to-string (stream)
    (loop for i across octet-vector
       do (flet ((foo (x) (aref hex-digits (ldb (byte x (- x 4)) i))))
            (princ (foo 8) stream)
            (princ (foo 4) stream)))))

(defun md5 (string)
  (octets->letters #+sbcl (sb-md5:md5sum-string string)
                   #-sbcl (with-input-from-string (stream string)
                            (md5:md5sum-stream stream))))

(defun sha3str (s)
  (format t "~(~{~2,'0x~}~)" (coerce (sha3:sha3-digest-vector (flexi-streams:string-to-octets s)) 'list)))

(defun ask-number (msg)
  (format t "~a: " msg)
  (let ((n (read)))
    (if (numberp n)
        n
        (ask-number msg))))

(defun collatz-list (x &optional lst)
  (if (> x 1)
      (collatz-list (if (evenp x)
                        (/ x 2)
                        (+ (* x 3) 1))
                    (cons x lst))
      (cons 1 lst)))

(defun collatz-walk (x)
  (format t "~{~d~%~}" (collatz-list x)))

(defun my-len (s &optional (n 0))
  (if (null s)
      n
      (my-len (cdr s) (+ n 1))))

(defun sum-factor (n)
  n)

(defun trunc-demo ()
  (let ((flst (list #'eval #'floor #'ceiling #'round #'truncate))
        (xlst '(0.1 0.5 0.6 1.0 1.1)))
    (setf xlst (append (mapcar #'- (reverse xlst)) '(0) xlst))
    (dolist (fun flst)
      (format t "~20a:" fun)
      (dolist (x xlst)
        (format t "~5,1f" (funcall fun x)))
      (format t "~%"))))

(defun prime-sieve (n)
  (let ((a (make-array (+ n 1) :initial-element t))
        (sieve nil))
    (loop for i from 2 to n do
         (when (aref a i)
           (push i sieve)
           (loop for k from (+ i i) to n by i do
                (setf (aref a k) nil))))
    (reverse sieve)))

(defun mysqrt (x &optional (r 1d0))
  (let ((y (* (+ (/ x r) r) 0.5)))
    (if (< (abs (- y r)) 1e-10)
        y
        (mysqrt x y))))

(defun mycopylist (lst)
  (if (atom lst)
      lst
      (cons (car lst) (mycopylist (cdr lst)))))

(defvar *gravity* 9.80665)

(defun falling-height (seconds)
  (* *gravity* seconds seconds 0.5))

(defun falling-speed (height)
  (sqrt (* 2 *gravity* height)))

(defun mynth (n lst)
  (if (> n 0)
      (mynth (- n 1) (cdr lst))
      (car lst)))

(defun mynthcdr (n lst)
  (if (> n 0)
      (mynth (- n 1) (cdr lst))
      lst))

(defun mylast (lst)
  (if (null (cdr lst))
      lst
      (mylast (cdr lst))))

(defun add-list (lst &optional (sum 0))
  (if (null lst)
      sum
      (add-list (cdr lst) (+ (car lst) sum))))

(defun mul-list (lst &optional (p 1))
  (if (null lst)
      p
      (add-list (cdr lst) (* (car lst) p))))

(defvar *test-tree* '(0 (1 ) (2)))

(defun mymember (x lst &optional (fn #'eql))
  (if (null lst)
      nil
      (if (funcall fn (car lst) x)
          lst
          (mymember x (cdr lst) fn))))

(defun mysubseq (lst begin &optional end n)
  lst
  begin
  end
  n)

(defun occurrences (lst)
  (let ((m nil))
    (dolist (e lst)
      (if (null (assoc e m))
          (push (cons e 1) m)
          (incf (cdr (assoc e m)))))
    (sort m #'> :key #'cdr)))

(defun occur-lt (a b)
  (< (cdr a) (cdr b)))

(defun properlistp (lst)
  (or (null lst)
      (and (consp lst)
           (properlistp (cdr lst)))))

(defun showdots (lst)
  (if (null lst)
      (format t "nil")
      (if (listp lst)
          (progn (format t "(")
                 (showdots (car lst))
                 (format t " . ")
                 (showdots (cdr lst))
                 (format t ")"))
          (format t "~a" lst))))

(defun pos+rec (lst &optional (n 0))
  (if (null lst)
      nil
      (cons (+ (car lst) n)
            (pos+rec (cdr lst) (+ n 1)))))

(defun empty (s)
  (zerop (length s)))

(defun split (s &optional lst)
  (let ((sp (position #\space s :from-end t)))
    (if (null sp)
        (if (empty s)
            lst
            (cons s lst))
        (let ((tail (subseq s (+ sp 1))))
          (split (subseq s 0 sp)
                 (if (empty tail)
                     lst
                     (cons tail lst)))))))

(defun range (begin end &optional lst)
  (if (< begin end)
      (let ((ne (- end 1)))
        (range begin ne (cons ne lst)))
      lst))
          
(defun coin-change (cent &optional (value 50) (coins nil))
  (cond ((= cent 0) (format t "~a~%" coins))
        ((< cent 0) nil)
        ((= value 0) nil)
        (t (coin-change (- cent value) value (cons value coins))
           (coin-change cent (cond ((= value 50) 20)
                                   ((= value 20) 10)
                                   ((= value 10) 5 )
                                   ((= value 5 ) 2 )
                                   ((= value 2 ) 1 )
                                   ((= value 1 ) 0 )) coins))))


(defun fibnacci (n)
  (cond ((< n 1) 0)
        ((= n 1) 1)
        (t (+ (fibnacci (- n 2))
              (fibnacci (- n 1))))))

(defun lg (x)
  (* (log x) 0.43429448190325176d0))

(defun combinatorial (n k)
  (/ (factorial2 n) (factorial2 k) (factorial2 (- n k))))

(defun genlist (max &key (min 0) (step 1))
  (loop for n from min below max by step
     collect n))

(defun pi-ramannujan (k)
  (labels ((item (n)
             (/ (* (factorial2 (* 4 n)) (+ 1103 (* 26390 n))) (* (expt (factorial2 n) 4) (expt 396 (* 4 n))))))
    (let ((sum 0))
      (loop for i from 0 to k do
           (setf sum (+ sum (item i))))
      (/ 9801 (* sum 2 (sqrt 2d0))))))

(defun my-funcall (fn &rest args)
  (apply fn args))

(defun printall (&rest args)
  (dolist (e args)
    (format t "~a~%" e)))

(defmacro myinc (v &optional (n 1))
  (list ))

(defun mkrng (n)
  (lambda ()
    (random n)))

(defun ackermann (m n)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (fixnum m n))
  (cond ((<= m 0) (+ n 1))
        ((= n 0) (ackermann (- m 1) 1))
        (t (ackermann (- m 1) (ackermann m (- n 1))))))

(defun foo (m)
  (let ((s "a"))
    (dotimes (n m)
      (setf s (concatenate 'string s s)))
    s))
      
(defun ff (x)
  (lambda (y) (* x y)))

(defparameter *my-fun* 1)

(defun my-func (v0)
  (setf (symbol-function '*my-fun*)
    (lambda (v1)
      (+ v0 v1)))
  '*my-fun*)

(defun tw-policy (tw)
  (dolist (e tw-jp-mg)
    (if (or (malep e) (oldp e))
        (terminate e)
        ((lambda (p)
          )
         (cons (pop soldiers) e)))))

(defun c2 (lst)
  (let ((a (car lst))
        (d (cdr lst)))
    (if (consp d)
        (append (mapcar (lambda (e) (cons a e)) d)
                (c2 d))
        nil)))

(defun cn (lst n)
  (let ((a (car lst))
        (d (cdr lst)))
    (if (> n 1)
        (append (mapcar (lambda (e) (cons a e))
                        (cn d (1- n)))
                (if (consp d) (cn d n) nil))
        (mapcar (lambda (e) (cons e nil)) lst))))

(defun estpi (cnt)
  (let ((n 0))
    (dotimes (i cnt)
      (let ((x (random 1.0))
            (y (random 1.0)))
        (when (<= (+ (* x x) (* y y)) 1.0)
          (incf n))))
    (/ (* n 4.0) cnt)))

(defun big-o (&optional (x 10) (y 1.01d0))
  (do* ((n 2 (* n 2))
        (a (expt n x) (expt n x))
        (b (expt y n) (expt y n)))
       ((< a b) n)
    (format t "~s^~s=~s ~s^~s=~s~%" n x a y n b)))
