;;;; Data Structures and Algorithms (using Common Lisp)

(defun square (x)
  (* x x))

;; bound should be integer, float, long-float
(defun random-array (n &key (bound 1.0) (rng *random-state*))
  (let ((a (make-array n)))
    (dotimes (i n)
      (setf (aref a i) (random bound rng)))
    a))

;; same as logcount
(defun count-ones-v1 (n)
  "Count bit ones in integer N, iterative version."
  (do ((m 0))
      ((zerop n) m)
    (if (plusp (logand n 1)) ; or (mod n 2)
        (incf m))
    (setf n (ash n -1))))

(defun count-ones-v2 (n &optional (ones 0))
  "Count bit ones in integer N, tail-recursive version."
  (if (zerop n)
      ones
      (count-ones-v2 (ash n -1)
                     (if (plusp (logand n 1))
                         (+ ones 1)
                         ones))))

(defun test-count-ones (&optional (cnt 65537))
  (dotimes (i cnt)
    (let ((a (list (logcount i) (count-ones-v1 i) (count-ones-v2 i))))
      (unless (apply #'= a)
        (format t "~s: ~s~%" i a)
        (return nil))))
  t)

(defun sum-array-v0 (a)
  (do ((n (array-dimension a 0))
       (i 0 (+ i 1))
       (s 0))
      ((>= i n) s)
    (incf s (aref a i))))

(defun sum-array-v1 (a)
  (loop for x across a sum x))

(defun sum-array-v2 (a)
  (reduce #'+ a))

(defun sum-array-v3 (a &optional (n (length a)) (i 0) (s 0))
  (if (>= i n)
      s
      (sum-array-v3 a n (1+ i) (+ (aref a i) s))))

(defun sum-array-v4 (a &optional (b 0) (e (- (length a) 1)))
  "divide-and-conquer"
  ;;(format t "~s ~s~%" b e)
  (cond ((< b e)
         (let ((m (+ b (ash (- e b) -1))))
           (+ (sum-array-v4 a b m)
              (sum-array-v4 a (1+ m) e))))
        ((= b e) (aref a b))
        (t 0)))

(defun test-sum-array (&optional (n 10))
  (let* ((a (random-array n :bound 100))
         (v (list (sum-array-v0 a)
                  (sum-array-v1 a)
                  (sum-array-v2 a)
                  (sum-array-v3 a))))
    (unless (apply #'= v)
      (format t "~s: ~s~%" a v)
      (return-from test-sum-array nil))
    t))

(defun fibonacci-v0 (n)
  (if (< n 2)
      n
      (+ (fibonacci-v0 (- n 2))
         (fibonacci-v0 (- n 1)))))

(defun fibonacci-tab (n &optional (ht (make-hash-table :test #'=)))
  (let ((x (gethash n ht)))
    (when (null x)
      (setf x (if (< n 2)
                  n
                  (+ (fibonacci-tab (- n 2) ht)
                     (fibonacci-tab (- n 1) ht))))
      (setf (gethash n ht) x))
    x))

(defun bubble-sort (a &optional (cmp #'<=))
  (let ((n (length a))
        (cmps 0)
        (swaps 0))
    (dotimes (i (1- n))
      (if (do ((sorted t) ;; 加速效果有限，甚至更慢
               (e (- n i))
               (j 0 (+ j 1))
               (k 1 (+ k 1)))
              ((>= k e) sorted)
            (incf cmps)
            (unless (funcall cmp (aref a j) (aref a k))
              (rotatef (aref a j) (aref a k))
              (incf swaps)
              (if sorted
                  (setf sorted nil)))
            ;;(format t "i=~a, e=~a, j=~a, k=~a~%" i e j k)
            )
          (return (values cmps swaps))))
    (values cmps swaps)))
