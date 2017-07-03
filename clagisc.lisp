(defvar objfeatures '(large shiny red cube -vs- small shiny red four-sided pyramid))

(defun right-side (lst)
  (cdr (member '-vs- lst)))

(defun left-side (lst)
  (right-side (reverse lst)))

(defun count-common (a b)
  (length (intersection a b)))

(defun obj-compare (lst)
  (let* ((ls (left-side lst))
         (rs (right-side lst))
         (cm (intersection ls rs)))
    (list (length cm) 'common 'features)))

(defvar animal-sounds '((cow . moo)
                        (pig . oink)))

(defvar staffs '((wangz1 15615316585 66108)
                 (dengli 15615316583 66107)))

(defun myseo (a b)
  (set-difference (union a b) (intersection a b)))

(defun swap-first-last (lst)
  (cons (car (reverse lst))
        (reverse (cons (car lst)
                       (cdr (reverse (cdr lst)))))))

(defun swap-first-last2 (lst)
  (let* ((a (reverse (rest lst)))
         (b (reverse (rest a))))
    (cons (first a)
          (append b (list (first lst))))))

(defun swap-first-last3 (lst)
  (let* ((a (reverse (rest lst))))
    (cons (car a)
          (reverse (cons (car lst) (rest a))))))


(defun rotate-left (lst)
  (reverse (cons (car lst)
                 (reverse (cdr lst)))))


(defun rotate-left-good (lst)
  (append (cdr lst)
          (list (car lst))))

(defun rotate-right (lst)
  (let ((rvs (reverse lst)))
    (cons (car rvs)
          (reverse (cdr rvs)))))

(defun find-join-cell (a b)
  (let ((n (- (length a)
              (length b))))
    (if (plusp n)
        (setf a (nthcdr n a))
        (setf b (nthcdr n b)))
    (labels ((aux (a b)
               (if (eq (car a) (car b))
                   a
                   (aux (cdr a) (cdr b)))))
      (aux a b))))

(defun roughly-equal (e k)
  (and (>= e (- k 10))
       (<= e (+ k 10))))


(defun find-first-roughly-equal (k lst)
  (find-if (lambda (e) (roughly-equal e k)) lst))

(defun find-nested (lst)
  (find-if #'consp lst))

(defun beforep (a b lst)
  (member b (member a lst)))

(defun rank (x) (first x))
(defun suit (x) (second x))

(defvar suits '(hearts clubs diamonds spades))
(defvar ranks '(2 3 4 5 6 7 8 9 jack queen king ace))

(defparameter my-hand '((3 hearts)
                        (5 clubs)
                        (2 diamonds)
                        (4 diamonds)
                        (ace spades)))

(defun count-suit (suit cards)
  (length (remove-if-not (lambda (e)
                           (equal suit (suit e)))
                         cards)))

(defparameter colors '((clubs black)
                       (diamonds red)
                       (hearts red)
                       (spades black)))


(defun color-of (cards)
  (second (assoc (suit cards) colors)))

(defun first-red (hand)
  (find-if (lambda (cards)
             (equal (color-of cards)
                    'red))
           hand))

(defun black-cards (hand)
  (remove-if-not (lambda (cards)
                   (equal (color-of cards) 'black))
                 hand))

(defun what-ranks (suit hand)
  (mapcar #'car (remove-if-not (lambda (cards)
                                 (equal (suit cards) suit))
                               hand)))

(defun higher-rank-p (acards bcards)
  (let ((ra (rank acards))
        (rb (rank bcards)))
    (member ra (cdr (member rb ranks)))))


(defun higher-rank-p-bad (acards bcards)
  (let ((ra (rank acards))
        (rb (rank bcards)))
    (beforep rb ra ranks)))

(defun high-card (hand)
  (assoc (find-if (lambda (rank)
                    (assoc rank hand))
                  (reverse ranks))
         hand))

(defun high-card2 (hand)
  (reduce (lambda (ac bc)
            (if (higher-rank-p ac bc) ac bc))
          hand))

(defun half (x)
  (* x 0.5))

(defun avg (&rest numbers)
  (/ (apply #'+ numbers)
     (length numbers)))

(defvar *blocks-world* 
  '((b1 shape brick)
    (b1 color green)
    (b1 size small)
    (b1 supported-by b2)
    (b1 supported-by b3)
    (b2 shape brick)
    (b2 color red)
    (b2 size small)
    (b2 supports b1)
    (b2 left-of b3)
    (b3 shape brick)
    (b3 color red)
    (b3 size small)
    (b3 supports b1)
    (b3 right-of b2)
    (b4 shape pyramid)
    (b4 color blue)
    (b4 size large)
    (b4 supported-by b5)
    (b5 shape cube)
    (b5 color green)
    (b5 size large)
    (b5 supports b4)
    (b6 shape brick)
    (b6 color purple)
    (b6 size large)))

(defun match-element (a b)
  (or (equal a b)
      (equal b '?)))

(defun match-triple (ta tb)
  (every #'match-element ta tb))

(defun fetch (ptn)
  (remove-if-not (lambda (tp)
                   (match-triple tp ptn))
                 *blocks-world*))

(defun color-ptn (name)
  `(,name color ?))

(defun supporters (name)
  (mapcar #'third (fetch (list name 'supported-by '?))))

(defun supp-cube (name)
  (find-if (lambda (nm)
             (fetch (list nm 'shape 'cube)))
           (supporters name)))

(defun desc-block (nm)
  (reduce #'append (mapcar #'cdr (fetch (list nm '? '?)))))

(defun merge-block-attribs ()
  (let ((ats '((b1 material woord)
               (b2 material plastic))))
    (mapcar (lambda (e)
              (unless (fetch e)
                (push e *blocks-world*)))
            ats)))


(defun alloddp (lst)
  (cond ((null lst) t)
        ((evenp (car lst)) nil)
        (t (alloddp (cdr lst)))))

(defun anyoddp (lst)
  (cond ((null lst) nil)
        ((oddp (car lst)) lst)
        (t (anyoddp (cdr lst)))))


(defun anyoddp2 (lst)
  (if (null lst)
      nil
      (if (oddp (car lst))
          lst
          (anyoddp2 (cdr lst)))))

(defun fact (x)
  (if (zerop x)
      1
      (* (fact (- x 1)) x)))

(defun fact-tr (n)
  (defun fact-tr-aux (i x)
    (if (<= i n)
        (fact-tr-aux (+ i 1) (* i x))
        x))
  (fact-tr-aux 1 1))

(defun laugh (n)
  (if (zerop n)
      nil
      (cons 'ha (laugh (- n 1)))))

(defun add-up (lst)
  (if lst
      (+ (car lst)
         (add-up (cdr lst)))
      0))

(defun rec-member (item lst)
  (cond ((null lst) nil)
        ((equal (car lst) item) lst)
        (t (rec-member item (cdr lst)))))

(defun rec-assoc (item lst)
  (cond ((null lst) nil)
        ((equal (caar lst) item) (car lst))
        (t (rec-assoc item (cdr lst)))))

(defun rec-nth (n list)
  (cond ((zerop n) (car list))
        (t (rec-nth (- n 1) (cdr list)))))

(defun rec-plus (x y)
  (if (plusp y)
      (rec-plus (1+ x) (1- y))
      x))

(defun fib (n)
  (if (> n 1)
      (+ (fib (- n 2))
         (fib (- n 1)))
      1))

(defun last-elm (lst)
  (if (consp (cdr lst))
      (last-elm (cdr lst))
      (car lst)))

(defun last-elm2 (lst)
  (if (atom (cdr lst))
      (car lst)
      (last-elm2 (cdr lst))))

(defun all-equal (lst)
  (if (consp (cdr lst))
      (and (equal (first lst) (second lst))
           (all-equal (cdr lst)))
      t))

(defun count-down (n)
  (if (plusp n)
      (cons n (count-down (- n 1)))
      nil))

(defun cmp-lst (a b)
  (if a
      (if b
          (cmp-lst (cdr a) (cdr b))
          1)
      (if b
          -1
          0)))

(defun my-remove (fn lst)
  (cond ((null lst) nil)
        ((funcall fn (car lst)) (my-remove fn (cdr lst)))
        (t (cons (car lst)
                 (my-remove fn (cdr lst))))))

(defun count-odd (lst)
  (cond ((null lst) 0)
        ((oddp (car lst))
         (+ (count-odd (cdr lst)) 1))
        (t (count-odd (cdr lst)))))

(defun count-odd2 (lst)
  (cond ((null lst) 0)
        (t (+ (count-odd2 (cdr lst))
              (if (oddp (car lst)) 1 0)))))

(defun my-intersect (a b)
  (cond ((null a) nil)
        ((member (car a) b)
         (cons (car a)
               (my-intersect (cdr a) b)))
        (t (my-intersect (cdr a) b))))

(defun count-atoms (lst)
  (cond ((atom lst) 1)
        (t (+ (count-atoms (car lst))
              (count-atoms (cdr lst))))))

(defun count-cons (lst)
  (cond ((atom lst) 0)
        (t (+ (count-cons (car lst))
              (count-cons (cdr lst))
              1))))

(defun sum-tree (root)
  (cond ((atom root) (if (numberp root) root 0))
        (t (+ (sum-tree (car root))
              (sum-tree (cdr root))))))

(defun my-subst (new old tree)
  (cond ((null tree) nil)
        ((atom tree) (if (equal tree old) new tree))
        (t (cons (my-subst new old (car tree))
                 (my-subst new old (cdr tree))))))

(defun flatten (lst)
  (cond ((null lst) nil) ; wrong
        ((consp lst) (append (flatten (car lst))
                             (flatten (cdr lst))))
        (t (list lst))))

(defun flatten2 (lst)
  (cond ((atom lst) (list lst))
        (t (append (flatten2 (car lst))
                   (and (cdr lst)
                        (flatten2 (cdr lst)))))))

(defun tree-depth (tree)
  (cond ((atom tree) 0)
        (t (+ (max (tree-depth (car tree))
                   (tree-depth (cdr tree)))
              1))))

(defun paren-depth (lst)
  (cond ((null lst) 1) ; ()
        ((atom lst) 0) ; 1 a
        ((and (car lst)
              (atom (car lst))) (paren-depth (cdr lst)))
        (t (+ (max (paren-depth (car lst))
                   (paren-depth (cdr lst)))
              1))))

(defun paren-depth2 (lst)
  (cond ((atom lst) 0) ; 1 a
        (t (max (+ (paren-depth2 (car lst)) 1)
                (paren-depth2 (cdr lst))))))

(defun count-up1 (n)
  (if (plusp n)
      (append (count-up1 (- n 1)) (list n))
      nil))

(defun count-up2 (n)
  (defun count-up2-aux (i)
    (if (<= i n)
        (cons i (count-up2-aux (+ i 1)))
        nil))
  (count-up2-aux 1))

(defun count-up-tr (n)
  (defun count-up-tr-aux (i lst)
    (if (<= i n)
        (count-up-tr-aux (+ i 1) (cons i lst))
        lst))
  (reverse (count-up-tr-aux 1 nil)))

(defun make-loaf (n)
  (if (plusp n)
      (cons 'x (make-loaf (- n 1)))
      nil))

(defun bury (sym n)
  (cond ((plusp n) (list (bury sym (- n 1))))
        (t sym)))

(defun pairings (a b)
  (if (and a b)
      (cons (list (car a)
                  (car b))
            (pairings (cdr a) (cdr b)))
      nil))

(defun sublists (a)
  (if a
      (cons a (sublists (cdr a)))
      nil))

(defun myreverse1 (x)
  (if x
      (append (myreverse1 (cdr x))
              (list (car x)))
      nil))

(defun myreverse (x)
  (defun myreverse-aux (a b)
    (if a
        (myreverse-aux (cdr a) (cons (car a) b))
        b))
  (myreverse-aux x nil))

(defun rec-union (a b)
  (cond ((null a) b)
        ((member (car a) b) (rec-union (cdr a) b))
        (t (rec-union (cdr a) (cons (car a) b)))))

(defun largest-even (a)
  (cond ((null a) 0)
        ((and (numberp (car a)) (evenp (car a)))
         (max (car a) (largest-even (cdr a))))
        (t (largest-even (cdr a)))))

(defun huge (n)
  (defun huge-aux (p m)
    (if (plusp m)
        (huge-aux (* p n) (- m 1))
        p))
  (huge-aux 1 n))

(defun every-other (lst)
  (if lst
      (cons (car lst) (every-other (cddr lst)))
      nil))

(defun left-half (lst)
  (defun left-half-aux (p pp)
    (if pp
        (cons (car p)
              (left-half-aux (cdr p) (cddr pp)))
        (cons (car p) nil)))
  (if lst
      (left-half-aux lst (cddr lst))
      nil))

(defun merge-lists (a b)
  (if a
      (if b
          (if (< (car a) (car b))
              (cons (car a)
                    (merge-lists (cdr a) b))
              (cons (car b)
                    (merge-lists a (cdr b))))
          a)
      b))

(defun tree-find-if (fn tree)
  (cond ((null tree) nil)
        ((consp (car tree))
         (or (tree-find-if fn (car tree))
             (tree-find-if fn (cdr tree))))
        ((funcall fn (car tree)) (car tree))
        (t (tree-find-if fn (cdr tree)))))

(defun tr-reverse (lst)
  (labels ((aux (a result)
             (if a
                 (aux (cdr a) (cons (car a) result))
                 result)))
    (aux lst nil)))

(defun arith-eval (expr)
  (if (numberp expr)
      expr
      (funcall (cadr expr)
               (arith-eval (car expr))
               (arith-eval (caddr expr)))))

(defun arith-eval-naive (expr)
  (if (numberp expr)
      expr
      (let* ((optr (cadr expr))
             (op1 (arith-eval (car expr)))
             (op2 (arith-eval (caddr expr)))
             (fn (cond ((equal optr '+) #'+)
                       ((equal optr '-) #'-)
                       ((equal optr '*) #'*)
                       ((equal optr '/) #'/))))
        (funcall fn op1 op2))))

(defun leagalp (expr)
  (or (numberp expr)
      (and (consp expr)
           (= (length expr) 3)
           (member (second expr) '(+ - * /))
           (leagalp (first expr))
           (leagalp (third expr)))))

(defun pfactor (n)
  (labels ((pfactor-aux (m i factors)
             (cond ((< m 2) factors)
                   ((= m i) (list m))
                   ((zerop (mod m i)) (append (pfactor-aux (/ m i) 2 nil)
                                              (pfactor-aux i 2 nil)
                                              factors))
                   (t (pfactor-aux m (+ i 1) factors)))))
    (pfactor-aux n 2 nil)))

(defun draw-line (n)
  (cond ((plusp n)
         (format t "*")
         (draw-line (- n 1)))
        (t (format t "~%"))))

(defun tic-tac-toe (a ofs)
  (let ((lst (mapcar (lambda (x) (if x x #\space)) a)))
    (format ofs " ~a | ~a | ~a ~%" (nth 0 lst)(nth 1 lst)(nth 2 lst))
    (format ofs "-----------~%")
    (format ofs " ~a | ~a | ~a ~%" (nth 3 lst)(nth 4 lst)(nth 5 lst))
    (format ofs "-----------~%")
    (format ofs " ~a | ~a | ~a ~%" (nth 6 lst)(nth 7 lst)(nth 8 lst))))


(defun space-over (n)
  (when (plusp n)
    (format t " ")
    (space-over (- n 1))))

(defun plot-one-point (msg y)
  (space-over y)
  (format t "~a~%" msg))

(defun plot-points (msg ylst)
  (when ylst
    (plot-one-point msg (car ylst))
    (plot-points msg (cdr ylst))))

(defun gen-series (from to &optional (step 1))
  (cond ((<= from to)
         (cons from (gen-series (+ from step) to step)))
        (t nil)))

(defun make-graph (func start end mark &optional (step 1) (yoffset 0))
  (plot-points mark
               (mapcar (lambda (x) (+ (funcall func x) yoffset))
                       (gen-series start end step))))

(defun square (x) (* x x))
(defun cube (x) (* x x x))

(defun eof-test ()
  (with-open-file (ofs "test2216.txt" :direction :output :if-exists :overwrite)
    (format ofs "~s ~s ~s ~s" 1 2.3 "ok" #\a))
  (with-open-file (ifs "test2216.txt")
    (labels ((readobj (eof)
               (let ((obj (read ifs nil eof)))
                 (unless (eq obj eof)
                   (print obj)
                   (readobj eof)))))
      (readobj (list 'foo)))))

(defun dot-prin1 (lst)
  (cond ((consp lst)
         (format t "(")
         (dot-prin1 (car lst))
         (format t " . ")
         (dot-prin1 (cdr lst))
         (format t ")"))
        (t (format t "~a" lst))))

(defun hybrid-prin1 (obj)
  (labels ((print-lst (lst)
             (hybrid-prin1 (car lst))
             (let ((d (cdr lst)))
               (cond ((null d) nil)
                     ((atom d)
                      (format t " . ")
                      (hybrid-prin1 d))
                     (t (format t " ")
                        (print-lst d))))))
    (cond ((consp obj)
           (format t "(")
           (print-lst obj)
           (format t ")"))
          (t (format t "~s" obj)))))

(defun nchop (lst)
  (setf (cdr lst) nil)
  lst)

(defun ntack (lst sym)
  (setf (cdr (last lst)) (cons sym nil))
  lst)

(setf (symbol-function 'nappend) #'nconc)
(setf (symbol-function 'nremove) #'delete)

(defun it-assoc (item alist)
  (dolist (e alist)
    (if (equal (car e) item)
        (return e))))

(defun it-length (lst)
  (let ((n 0))
    (dolist (e lst)
      (incf n))
    n))

(defun it-nth (n lst)
  (dolist (e lst)
    (if (zerop n)
        (return e))
    (decf n)))

(defun it-nth-v2 (n lst)
  (dotimes (i n (car lst))
    (pop lst)))

(defun it-union (a b)
  (let ((u a))
    (dolist (e b)
      (if (not (member e u))
          (push e u)))
    u))

(defun it-reverse (lst)
  (let ((a nil))
    (dolist (e lst)
      (push e a))
    a))

(defun do-check-all-odd (lst)
  (do ((a lst (cdr a)))
      ((null a) t)
    (if (evenp (car a))
        (return nil))))

(defun ffo-with-do (x)
  (do ((z x (rest z))
       (e (first x) (first z))
       (dummy (format t "z: ~a, e: ~a~%" x (first x)) (format t "z: ~a, e: ~a~%" (rest z) (first z))))
      ((null z) nil)
    (if (oddp e) (return e))))

(defun do-fib (n)
  (do ((i 1 (+ i 1))
       (a 1 b)
       (b 1 (+ a b)))
      ((>= i n) b)))

; 11.22
(defun complement-base (base)
  (cond ((eq base 'a) 't)
        ((eq base 't) 'a)
        ((eq base 'g) 'c)
        (t 'g)))

(defun complement-base-v2 (base)
  (second (assoc base '((a t) (t a) (g c) (c g)))))

(defun complement-strand (strand)
  (mapcar #'complement-base strand))

(defun complement-strand-it (strand)
  (do ((cmp nil)
       (src strand (cdr src)))
      ((null src) (nreverse cmp))
    (push (complement-base (car src)) cmp)))

(defun complement-strand-it-v2 (strand)
  (do ((s strand (cdr s))
       (cmp nil (cons (car s) cmp)))
      ((null s) (nreverse cmp))))

(defun make-double (strand)
  (do ((s strand (cdr s))
       (r nil (cons (list (car s)
                          (complement-base (car s)))
                    r)))
      ((null s) (nreverse r))))

(defun count-bases-strand (strand)
  (do* ((na 0)
        (nt 0)
        (ng 0)
        (nc 0)
        (s strand (cdr s))
        (b (car s) (car s)))
       ((null s) `((a ,na) (t ,nt) (g ,ng) (c ,nc)))
    (cond ((eq b 'a) (incf na))
          ((eq b 't) (incf nt))
          ((eq b 'g) (incf ng))
          ((eq b 'c) (incf nc)))))

;;;; fixme: 前次运行的结果被保留了，应该每次创建新的cnt列表。要弄清原因
(defun count-bases-strand-v2 (strand)
  (do ((cnt '((a 0) (t 0) (g 0) (c 0)))
       (s strand (cdr s)))
      ((null s) cnt)
    (print cnt)
    (incf (second (assoc (car s) cnt)))))

(defun count-bases-helix (helix)
  (do ((cnt '((a 0) (t 0) (g 0) (c 0)))
       (s helix (cdr s)))
      ((null s) cnt)
    (incf (second (assoc (first (car s)) cnt)))
    (incf (second (assoc (second (car s)) cnt)))))


(defun count-bases (bases)
  (if (atom (car bases))
      (count-bases-strand-v2 bases)
      (count-bases-helix bases)))

(defun footest ()
  (do ((cnt '(1)))
      (nil)
    (print cnt)
    (incf (car cnt))
    (return cnt)))

(defun lsttest ()
  (list 1 2 3))

(defun dna-prefixp (a b)
  (do ((pa a (cdr pa))
       (pb b (cdr pb)))
      ((null pa) t)
    (unless (eq (car pa) (car pb))
        (return nil))))

(defun dna-appearsp (a b)
  (do ((pb b (cdr pb)))
      ((null pb) (null a))
    (if (dna-prefixp a pb)
        (return t))))

(defun dna-coverp (a b)
  (do ((pb b))
      ((null pb) t)
    (do ((pa a (cdr pa)))
        ((null pa) nil)
      (unless (eq (car pa) (car pb))
        (return-from dna-coverp nil))
      (setf pb (cdr pb)))))

(defun dna-coverp-v2 (a b)
  (do* ((n (length a))
        (pb b (nthcdr n pb)))
       ((null pb) t)
    (unless (dna-prefixp a pb)
      (return nil))))

(defun dna-prefix (n strand)
  (do ((s strand (cdr s))
       (r nil (cons (car s) r))
       (i 0 (+ i 1)))
      ((>= i n) (reverse r))))

(defun dna-kernel (strand)
  (do* ((n (length strand))
        (i 1 (+ i 1))
        (p (dna-prefix i strand) (dna-prefix i strand)))
       ((>= i n) strand)
    (if (dna-coverp p strand)
        (return-from dna-kernel p))))

(defun dna-kernel-v2 (strand)
  (do ((i 1 (+ i 1)))
      ((dna-coverp (dna-prefix i strand) strand)
       (dna-prefix i strand))))

(defun dna-kernel-v3 (strand)
  (do* ((i 1 (+ i 1))
        (p (dna-prefix i strand) (dna-prefix i strand)))
       ((dna-coverp p strand) p)))

(defun dna-draw (a)
  (let ((b (complement-strand a)))
    (dolist (e b) (format t "-----"))(format t "~%")
    (dolist (e b) (format t "  !  "))(format t "~%")
    (dolist (e a) (format t "  ~a  " e))(format t "~%")
    (dolist (e b) (format t "  .  "))(format t "~%")
    (dolist (e b) (format t "  .  "))(format t "~%")
    (dolist (e b) (format t "  ~a  " e))(format t "~%")
    (dolist (e b) (format t "  !  "))(format t "~%")
    (dolist (e b) (format t "-----"))(format t "~%")))

(defun my-prog1 (x &rest ignore) x)
(defun my-prog2 (x y &rest ignore) y)
(defun my-progn (&rest x) (car (last x)))


;; 12.4
(defstruct adnode
  name question ycase ncase)

(defvar *adnodes* nil)

(defun ad-init ()
  (setf *adnodes* nil)
  'initialized)

(defun ad-add-node (name question ycase ncase)
  (push (make-adnode :name name :question question :ycase ycase :ncase ncase) *adnodes*)
  name)

(defun ad-find-node (name)
  (dolist (node *adnodes*)
    (if (equal (adnode-name node) name)
        (return node))))

(defun ad-find-node-v2 (name)
  (find-if (lambda (node) (equal (adnode-name node) name))
           *adnodes*))

(defun ad-process-node (name)
  (let ((node (ad-find-node name)))
    (when (null node)
      (format t "~&node ~a: undefined" node)
      (return-from ad-process-node nil))
    (format t "~a" (adnode-question node))
    (if (y-or-n-p) (adnode-ycase node) (adnode-ncase node))))

(defun ad-run ()
  (do ((current-node 'start (ad-process-node current-node)))
      ((null current-node) nil)
    (format t "current-node: ~a~%" current-node)
    (when (stringp current-node)
      (format t "~&~a" current-node)
      (return nil))))

(ad-init)
(ad-add-node 'start "turn-over?" 'turn-over 'wont-turn-over)
(ad-add-node 'turn-over "run-briefly?" 'engine-run-briefly 'engine-wont-run)

(defun print-starship (obj stream depth)
  (if (< depth 5)
      (format stream "#<STARSHIP ~a with ~a>" (starship-name obj) (starship-captain obj))))

(defstruct (starship (:print-function print-starship))
  name speed captain)

(defun print-captain (obj stream depth)
  (if (< depth 5)
      (format stream "#<Captain ~a@~a>" (captain-name obj) (captain-ship obj))))

(defstruct (captain (:print-function print-captain))
  name ship)

(defstruct animal
  name)

(defstruct (tiger (:include animal)))
(defstruct (lion (:include animal)))
;(defstruct (liger (:include tiger) (:include lion)))

(defun array-plist-hashtable-compare (&key (cnt 100))
  (let* ((a (make-array cnt :element-type 'float))
         (h (make-hash-table))
         (k 1000)
         (c 100000)
         (v (make-array k)))
    (dotimes (i k)
      (let ((n (random cnt))
            (s (sin (random 1.0))))
        (setf (aref v i) n)
        (setf (aref a n) s)
        (setf (gethash n h) s)))
    (print (time (loop for m from 0 to c
                       sum (do* ((i 0 (+ i 1))
                                 (n (aref v i))
                                 (s 0 (+ (aref a n) s)))
                                ((>= i k) s)))))
    (print (time (loop for m from 0 to c
                       sum
                       (do* ((i 0 (+ i 1))
                             (n (aref v i))
                             (s 0 (+ (gethash n h) s)))
                            ((>= i k) s)))))))

(defun random-array (size range)
  (let ((a (make-array size)))
    (dotimes (i size)
      (setf (aref a i) (random range)))
    a))

;;13.8
(defparameter *hist-array* nil)
(defparameter *total-points* 0)

(defun randtest-new-histogram (size)
  (defparameter *total-points* 0)
  (defparameter *hist-array* (make-array size)))

(defun randtest-record-value (n)
  (if (< n (length *hist-array*))
      (progn
        (incf *total-points*)
        (incf (aref *hist-array* n)))
      (format t "~&out of range: ~a~%" n)))

(defun randtest-sample (n)
  (let ((bins (length *hist-array*)))
    (dotimes (i n) (randtest-record-value (random bins)))))

(defun randtest-print-histline (n &optional (scale 1))
  (let ((cnt (aref *hist-array* n)))
    (format t "~3d [~6d]" n cnt)
    (dotimes (i (round (* cnt scale))) (format t "*"))
    (format t "~%")))

(defun randtest-print-histgram ()
  (let* ((max (reduce #'max *hist-array*))
         (scale (/ 80.0 max)))
    (dotimes (i (length *hist-array*))
      (randtest-print-histline i scale))))

;; 13.9
(defparameter *crypto-text* '("ej ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
                              "enlpo pib slafml pvv bfwkj"))

(defparameter *encipher-tbl* (make-hash-table))
(defparameter *decipher-tbl* (make-hash-table))

(defun make-subst (src-char dst-char)
  (setf (gethash src-char *encipher-tbl*) dst-char)
  (setf (gethash dst-char *decipher-tbl*) src-char))

(defun undo-subst (char)
  (setf (gethash char *encipher-tbl*) nil)
  (setf (gethash char *decipher-tbl*) nil))

(defun cipher-string (s)
  (map 'string (lambda (c)
                 (let ((x (gethash c *encipher-tbl*)))
                   (if x x #\space)))
       s))

(defmacro my-incf (x &optional (amount 1))
  (list 'setf x (list '+ x amount)))

;; 14.5
(defmacro set-mutual (a b)
  `(progn
     (setf ,a ',b)
     (setf ,b ',a)))

(defmacro set-zero (&rest vars)
  `(progn
     ,@(mapcar (lambda (x) (list 'setf x 0))
               vars)
     '(zeroed ,@vars)))

(defmacro var-chain (&rest vars)
  (do* ((a vars b)
        (b (cdr a) (cdr b))
        (r nil ))
       ((null b) (cons 'progn (reverse r)))
    (setf r (cons `(setf ,(car a) ',(car b)) r))))
