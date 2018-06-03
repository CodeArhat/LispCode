(defvar ht (make-hash-table))

(defun ． ()
  (when (null (gethash 0 ht))
    (setf (gethash 0 ht) 123)
    (format t "~s" "祖国！")
    (return-from ． 0))
  "万岁！")

(defparameter mds '((1 . 9) (1 . 20) (1 . 31) (2 . 1) (2 . 15) (2 . 29) (3 . 11) (3 . 21) (3 . 31) (4 . 18) (4 . 19) (4 . 20) (5 . 9) (5 . 19) (5 . 29) (6 . 15) (6 . 18) (6 . 21)))

(defun count-cdr (alist x)
  (length (loop for e in alist
                when (= (cdr e) x)
                  collect 1)))

(defun count-elts (alist)
  (let ((ds (sort (remove-duplicates (mapcar #'cdr alist)) #'<)))
    (mapcar (lambda (x)
              (cons x (count-cdr alist x)))
            ds)))


(defparameter *graph*
  '((a b c d e g i) ; from car point to each cdr
    (b c d e h f)
    (c d e g i)
    (d e f h)
    (e f g i h)
    (f g h i)
    (g h i)
    (h i)))

(defparameter *lines*
  '((a c e g i)
    (b c d)
    (b e h)
    (d e f)
    (f g h)))

(defun list-points (graph)
  (sort (remove-duplicates (apply #'append graph))
        #'string<))

(defun gen-combinations (lst n)
  (if (and lst (> n 1))
      (append (mapcar (lambda (e) (cons (car lst) e))
                      (gen-combinations (cdr lst) (1- n)))
              (gen-combinations (cdr lst) n))
      (if (= n 1)
          (mapcar #'list lst))))

(defun edge-in (edge graph)
  (let ((pa (first edge))
        (pb (second edge)))
    (some (lambda (g)
            (and (eq pa (car g))
                 (member pb (cdr g))))
          graph)))

(defun triple-in (triple graph)
  (every (lambda (edge) (edge-in edge graph))
         (gen-combinations triple 2)))

(defun on-line (triple lines)
  (some (lambda (line) (subsetp triple line))
        lines))

(defun list-triangles (graph lines)
  (remove-if-not (lambda (triple)
                   (and (triple-in triple graph)
                        (not (on-line triple lines))))
                 (gen-combinations (list-points graph) 3)))

(defun oracle-rdbms-test ()
  (ql:quickload :hu.dwim.perec.oracle)
  (ql:quickload :hu.dwim.rdbms.oracle)
  )

(defun oracle-plain-odbc-test ()
  (ql:quickload :plain-odbc)
  (let* ((db (plain-odbc:connect "JYFX" "jyfx" "jyfx123"))
         (s (caar (plain-odbc:exec-query db "select username||'_abc_' as foo from xfb_auth where sid = 0")))
         (n (length s))
         (bs (make-array n :element-type '(unsigned-byte 8))))
    (print (plain-odbc:exec-query db "select * from v$nls_parameters"))
    (dotimes (i n)
      (setf (aref bs i) (char-code (char s i))))
    (wz:utf8decode bs)))

(defun oracle-clsql-odbc-test ()
  (ql:quickload :clsql-odbc)
  (let* ((db (clsql:connect '("JYFX" "jyfx" "jyfx123")
                            :database-type :odbc
                            :if-exists :new
                            :make-default nil
                            :encoding :utf-8))
         (s (caar (clsql:query "select username||'_abc_十九大' as foo from xfb_auth where sid = 0"
                               :database db)))
         (n (length s))
         (bs (make-array n :element-type '(unsigned-byte 8))))
    (print (clsql:query "select * from v$nls_parameters" :database db))
    (dotimes (i n)
      (setf (aref bs i) (char-code (char s i))))
    (clsql:disconnect :database db)
    (wz:utf8decode bs)))

(defun logic3 ()
  (remove-if
   (lambda (a)
     (let ((x (first a))
           (y (second a))
           (z (third a)))
       (> (gcd (gcd x y) z) 1)))
   (wz::gen-combinations '(1 2 3 4 5 6 7 8 9) 3)))

(defun sp-test ()
  (let ((a (loop for x from 0 to 1000 collect (write-to-string x)))
        (b nil))
    (loop repeat 100 do
      (setf b (mapcar #'wz:sha3s a)))
    (reduce (lambda (x y)
              (wz:sha3s (concatenate 'string x y)))
            b)))

(defparameter *stk* nil)

(defun random-stack (&key (count 10) (max 100))
  (setf *stk* nil)
  (loop repeat count do
    (push (random max) *stk*)))

(defun pop-min ()
  (when *stk*
    (let ((a (pop *stk*)))
      (if (cdr *stk*)
          (if (< (cdr *stk*) a)
              (progn
                (let ((b (pop *stk*)))
                  (push a *stk*)
                  (push b *stk*)
                  (format t "top-swapped: ~s~%" *stk*)
                  ))
              (push a *stk*))
          a))))

(defun sort-stack ()
  (format t "arg: ~s~%" *stk*)
  (when (and *stk* (cdr *stk*)) ; 2 elements at least
    (let ((a (pop *stk*)))
      (sort-stack)
      (format t "cdr-sorted: ~s~%" *stk*)
      (if (< (car *stk*) a)
          (progn
            (let ((b (pop *stk*)))
              (push a *stk*)
              (push b *stk*)
              (format t "top-swapped: ~s~%" *stk*)
              ))
          (push a *stk*)))))
