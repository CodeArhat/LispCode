#|
Wang Zheng's Common Lisp Utilities.

# Parameter Naming Rules:

a, b, c, d: atom
e: element
f: function
g: general-object
h: hash-table
i, j, k: integer
l, m, n: natural (>= 0)
o, p, q: object (atom or list)
r: rational
s: string
u, v, w: vector
x, y, z: number (int or float)

bs: bytes
cs: database-cursor
db: database-connection
fn: function
ht: hash-table
rc: record
rs: record-set
tx: database-transaction

arr: array
err: error-condition
ist, ost: input/output stream
ifs, ofs: input/output file stream
iss, oss: input/output string stream
jsn: json
lst: list
obj: object (atom or list)
pth: path
seq: sequence (list, array, string)
str: string
tmp: temporary
tms: timestamp
utm: unix-time
val: value
vec: vector
ymd: year-month-day (YYYYMMDD)

s...: source
t...: target

# Encoding: utf8 only
|#

(in-package :com.zlisp.wz)

(ql:quickload :cl-base64)
(ql:quickload :cl-fad)
(ql:quickload :cl-json)
(ql:quickload :cl-ppcre)
(ql:quickload :cl-smtp)
(ql:quickload :cl-syntax-annot)
(ql:quickload :cl+ssl)
(ql:quickload :clsql-odbc)
(ql:quickload :css-selectors)
(ql:quickload :cxml)
(ql:quickload :djula)
(ql:quickload :datafly)
(ql:quickload :do-urlencode)
(ql:quickload :drakma)
(ql:quickload :envy)
(ql:quickload :flexi-streams)
(ql:quickload :ironclad)
(ql:quickload :lack)
(ql:quickload :local-time)
(ql:quickload :md5)
(ql:quickload :parse-float)
(ql:quickload :parse-number)
(ql:quickload :plain-odbc)
(ql:quickload :postmodern)
(ql:quickload :random-state)
(ql:quickload :regex)
(ql:quickload :secure-random)
(ql:quickload :sha3)
(ql:quickload :sxql)
(ql:quickload :trivial-utf-8)
(ql:quickload :uiop)
(ql:quickload :yason)

;; math
(defun square (x)
  (* x x))

(defun cube (x)
  (* x x x))

(defun cbrt (x)
  (expt x 1/3))

(defun fibonacci (n)
  (do ((i 1 (+ i 1))
       (a 1 b)
       (b 1 (+ a b)))
      ((>= i n) b)))

(defun fibonacci-tab (n &optional (ht (make-hash-table )))
  (let ((x (gethash n ht)))
    (when (null x)
      (setf x (if (< n 2)
                  n
                  (+ (fibonacci-tab (- n 2) ht)
                     (fibonacci-tab (- n 1) ht))))
      (setf (gethash n ht) x))
    x))

(defun fibonacci-v0 (n)
  (if (< n 2)
      n
      (+ (fibonacci-v0 (- n 2))
         (fibonacci-v0 (- n 1)))))

(defun factorial-v1 (n)
  (if (> n 1)
      (* (factorial-v1 (- n 1)) n)
      1))

(defun factorial-v2 (n &optional (p 1))
  (if (> n 1)
      (factorial-v2 (- n 1) (* p n))
      p))

(defun factorial-v3 (n)
  (let ((p 1))
    (loop for i from 1 to n do
          (setf p (* p i)))
    p))

(defun ackermann (m n)
  (declare (optimize (speed 3) (space 0) (debug 0) (safety 0) (compilation-speed 0)))
  (declare (fixnum m n))
  (the fixnum (if (<= m 0)
                  (+ n 1)
                  (if (<= n 0)
                      (ackermann (- m 1) 1)
                      (ackermann (- m 1) (ackermann m (- n 1)))))))

(defun accum-int (m n)
  (/ (* (+ m n)
        (- n m -1))
     2))

(defun accum-natural (n)
  (/ (* n (1+ n)) 2))

(defun accum-square (n)
  (/ (* n (1+ n) (1+ (* n 2))) 6))

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

;; conditions
(define-condition zerror (error)
  ((code :initarg :code :reader ecode)
   (info :initarg :info :reader einfo))
  (:report (lambda (err ost)
             (format ost "~a" (einfo err))))) 

(defmacro raise (msg)
  `(error (make-condition 'zerror :info ,msg)))

(defun nvl (a b)
  (if a a b))

(defun nvls (a b)
  (if (or (null a)
          (string= a ""))
      b
      a))

(defun empty-to-null (str)
  (if (string= str "") :null str))

;;; char functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string functions
(defun toupper (s)
  (string-upcase s))

(defun tolower (s)
  (string-downcase s))

(defun trim (s &optional (chars '(#\space)))
  (string-trim chars s))

(defun ltrim (s &optional (chars '(#\space)))
  (string-left-trim chars s))

(defun rtrim (s &optional (chars '(#\space)))
  (string-right-trim chars s))

(defun strcat (a b)
  (concatenate 'string a b))


(defun split (s &optional (regexp-delimiter " +"))
  (cl-ppcre:split regexp-delimiter (trim s)))

(defun replace-all (s regex replacement)
  (cl-ppcre:regex-replace-all regex s replacement))


(defun utf8decode (bytes)
  (trivial-utf-8:utf-8-bytes-to-string bytes))

(defun utf8encode (str)
  (trivial-utf-8:string-to-utf-8-bytes str))

(defun utf8read (istream)
  (setf (flexi-streams:flexi-stream-external-format istream) :utf-8)
  (trivial-utf-8:read-utf-8-string istream :stop-at-eof t))

(setf yason:*parse-json-null-as-keyword* t)
(setf yason:*parse-json-booleans-as-symbols* t)
(setf yason:*parse-json-arrays-as-vectors* t)

(defun jsondecode (str)
  (yason:parse str))

(defun jsonencode (jsn)
  (let ((oss (make-string-output-stream)))
    (yason:encode jsn oss)
    (get-output-stream-string oss)))

(defun stoi (s)
  (parse-integer s))

(defun itos (i)
  (write-to-string i))

(defun tostr (a)
  (format nil "~a" a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time functions
(defun now ()
  (local-time:now))

(defun unixtime ()
  (local-time:timestamp-to-unix (now)))

(defun format-time (tm fmt)
  (format nil fmt
          (mod (local-time:timestamp-year tm) 100)
          (local-time:timestamp-month tm)
          (local-time:timestamp-day tm)))

;; YYYYmmdd
(defvar +yyyymmdd-format+ '((:year 4) (:month 2) (:day 2)))

;; YYYYmmddHHMMSS
(defvar +ymdhms-format+ '((:year 4) (:month 2) (:day 2) (:hour 2) (:min 2) (:sec 2)))

;; YYYY-mm-ddTHH:MM:SS.MIL+TIMEZONE
(defvar +iso-tms-ms-format+ '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\T
                              (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2) #\. (:MSEC 3)
                              :GMT-OFFSET-OR-Z))

(defun ymdhms ()
  (local-time:format-timestring nil (local-time:now) :format wz::+ymdhms-format+))

(defun ymdhms-num ()
  (parse-integer (ymdhms)))

(defun yymmdd ()
  (format-time (now) "~2,0d~2,'0d~2,'0d"))

(defun yyyymmdd ()
  (local-time:format-timestring nil (local-time:now) :format +yyyymmdd-format+))

(defun yyyy-mm-dd ()
  (local-time:format-timestring nil (local-time:now) :format local-time:+iso-8601-date-format+))

(defun iso-tms-ms ()
  (local-time:format-timestring nil (local-time:now) :format wz::+iso-tms-ms-format+))

(defun iso-tms-us ()
  (local-time:format-timestring nil (local-time:now)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bitwise functions

(defun shl (x width bits)
  "Compute bitwise left shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x bits)
          (1- (ash 1 width))))

(defun shr (x width bits)
  "Compute bitwise right shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x (- bits))
          (1- (ash 1 width))))

(defun rotl (x width bits)
  "Compute bitwise left rotation of x by 'bits' bits, represented on 'width' bits"
  (logior (logand (ash x (mod bits width))
                  (1- (ash 1 width)))
          (logand (ash x (- (- width (mod bits width))))
                  (1- (ash 1 width)))))

(defun rotr (x width bits)
  "Compute bitwise right rotation of x by 'bits' bits, represented on 'width' bits"
  (logior (logand (ash x (- (mod bits width)))
                  (1- (ash 1 width)))
          (logand (ash x (- width (mod bits width)))
                  (1- (ash 1 width)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hash functions
(defun md5s (s)
  (ironclad:byte-array-to-hex-string (md5:md5sum-sequence (wz:utf8encode s))))

(defun md5su (s)
  (toupper (md5s s)))

(defun sha1s (s)
  (let* ((bs (flexi-streams:string-to-octets s :external-format :utf-8))
         (dg (ironclad:digest-sequence :sha1 bs))
         (ss (ironclad:byte-array-to-hex-string dg)))
    ss))

(defun sha1su (s)
  (toupper (sha1s s)))

(defun sha256s (s)
  (let* ((bs (flexi-streams:string-to-octets s :external-format :utf-8))
         (dg (ironclad:digest-sequence :sha256 bs))
         (ss (ironclad:byte-array-to-hex-string dg)))
    ss))

(defun sha256su (s)
  (toupper (sha256s s)))

(defun sha3s (s)
  (let* ((bs (flexi-streams:string-to-octets s :external-format :utf-8))
         (dg (sha3:sha3-digest-vector bs)))
    (ironclad:byte-array-to-hex-string dg)))

(defun sha3s-v1 (s)
  (format nil "~(~{~2,'0x~}~)" (coerce (sha3:sha3-digest-vector (flexi-streams:string-to-octets s)) 'list)))

(defun sha3su (s)
  (toupper (sha3s s)))

(defun password-hash (salt pswd)
  (sha3s (strcat salt pswd)))

(defun password-check (salt pswd hash)
  (string= (password-hash salt pswd) hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random functions
(defun randnum (b e)
  (+ (secure-random:number (- e b)) b))

(defun randbs62 (n)
  ""
  (let ((s (subseq (wz:replace-all
                    (base64:usb8-array-to-base64-string
                     (secure-random:bytes (+ (* n 2) 10) secure-random:*generator*))
                    "\\+|/|=" "")
                   0 n)))
    (if (= (length s) n)
        s
        (randbs62 n))))

(defun randnum4 () (randnum 1000     10000    ))
(defun randnum6 () (randnum 100000   1000000  ))
(defun randnum8 () (randnum 10000000 100000000))

(defun randbit32 ()
  (secure-random:number 4294967296))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; database functions
(defun qrecord (db sql args)
  (format t "~a, ~a, ~a~%" db sql args))

(defun qvalue (db sql args )
  (format t "~a, ~a, ~a~%" db sql args))

(defun qset (db sql args)
  (format t "~a, ~a, ~a~%" db sql args))

(defun gen-char-list (cb ce)
  (loop for c from (char-code cb) to (char-code ce)
       collect (string (code-char c))))

(defun pctest (s n)
  (do ((i 0 (+ i 1))
       (h s (md5s h)))
      ((>= i n) h)))

(defun pctest-sp (n cb ce)
  (md5s (reduce #'strcat
                (map 'list
                     (lambda (s) (pctest s n))
                     (gen-char-list cb ce)))))

;;(defun pctest-mp (n cb ce)
;;  (md5s (reduce #'strcat
;;                (map 'list
;;                     #'sb-thread:join-thread
;;                     (map 'list
;;                          (lambda (s)
;;                            (sb-thread:make-thread #'pctest :arguments `(,s ,n)))
;;                          (gen-char-list cb ce))))))

(defun demo-write-text-file ()
  (with-open-file (ofs "test.txt" :direction :output :if-exists :supersede)
    (dotimes (n 10)
      (format ofs "~5,'0d~%" n))))

(defun dump-array (a)
  (dotimes (i (length a))
    (format t "[~a]: ~a~%" i (aref a i))))

(defmacro geth (hashtbl key)
  `(gethash ,key ,hashtbl))

(defmacro seth (hashtbl key value)
  `(setf (gethash ,key ,hashtbl) ,value))

(defun dump (obj &optional (indent "") (skipfirst t))
  (labels ((dump-ht (ht indent skipfirst)
             (when (zerop (hash-table-count ht))
               (format t "~a{}" (if skipfirst "" indent))
               (return-from dump-ht))
             (format t "~a{~%" (if skipfirst "" indent))
             (loop for k being the hash-keys of ht do
               (format t "~a  (~s . " indent k)
               (dump (gethash k ht) (wz:strcat indent "  ") t)
               (format t ")~%"))
             (format t "}" ))

           (dump-cons (c indent)
             (if c
                 (let ((a (car c))
                       (d (cdr c)))
                   (dump a)
                   (if (consp d)
                       (progn (format t " ")
                              (dump-cons d indent))
                       (progn (when d
                                (format t ". ")
                                (dump d))
                              (format t ")"))))
                 (format t ")"))))

    (cond ((hash-table-p obj)
           (dump-ht obj indent skipfirst))
          ((consp obj)
           (format t "~a(" indent)
           (dump-cons obj indent))
          (t (format t "~s" obj)))))

(defun pkcs7padding (data blockbytes)
  "按PKCS7标准对数据进行填充。"
  (let* ((n (length data))
         (rn (mod n blockbytes))
         (pn (if (zerop rn) blockbytes (- blockbytes rn)))
         (buf (make-array (+ n pn) :element-type '(unsigned-byte 8)))
         )
    (dotimes (i n)
      (setf (aref buf i) (aref data i)))
    (dotimes (i pn)
      (setf (aref buf (+ n i)) pn))
    buf))

(defun pkcs7trim (data)
  (let* ((n (length data))
         (pn (aref data (- n 1)))
         (buf (make-array (- n pn) :element-type '(unsigned-byte 8))))
    (dotimes (i (- n pn))
      (setf (aref buf i) (aref data i)))
    buf))

(defun aes-encrypt-blocks (key plainblocks &optional (mode :ecb))
  "ToDo: CBC/CFB/OFB modes"
  (let* ((kbytes (utf8encode key))
         (cblocks (make-array (length plainblocks) :element-type '(unsigned-byte 8)))
         (cipher (ironclad:make-cipher 'ironclad:aes :key kbytes :mode mode)))
    ;;(format t "kbytes:~a~%" kbytes)
    (ironclad:encrypt cipher plainblocks cblocks)
    cblocks))

(defun aes-decrypt-blocks (key cipherblocks &optional (mode :ecb))
  (let* ((kbytes (utf8encode key))
         (dblocks (make-array (length cipherblocks) :element-type '(unsigned-byte 8)))
         (cipher (ironclad:make-cipher 'ironclad:aes :key kbytes :mode mode)))
    ;;(format t "kbytes:~a~%" kbytes)
    (ironclad:decrypt cipher cipherblocks dblocks)
    dblocks))

(defun aes-encrypt-bytes (key plainbytes &optional (mode :ecb))
  (let* ((pblocks (pkcs7padding plainbytes 16))
         (cblocks (aes-encrypt-blocks key pblocks mode)))
    cblocks))

(defun aes-decrypt-bytes (key cipherblocks)
  (let* ((dblock (aes-decrypt-blocks key cipherblocks))
         (dbytes (pkcs7trim dblock)))
    dbytes))

(defun aes-encrypt-str (key plaintext)
  "encrypt utf8 string, then encode result in base64 format."
  (assert (member (length key) '(16 24 32)))
  (let* ((pbytes (wz:utf8encode plaintext))
         (cblocks (aes-encrypt-bytes key pbytes))
         (ctext (base64:usb8-array-to-base64-string cblocks)))
    ctext))

(defun aes-decrypt-str (key ciphertext)
  "decrypt base64 encoded ciphertext and decode as utf8 string."
  (assert (member (length key) '(16 24 32)))
  (let* ((cblocks (base64:base64-string-to-usb8-array ciphertext))
         (dbytes (aes-decrypt-bytes key cblocks))
         (dtext (utf8decode dbytes)))
    dtext))

(defun xml-dom (s)
  (cxml:parse s (cxml-dom:make-dom-builder)))

(defun dom-node-value (node path)
  (dom:node-value
   (dom:first-child ;; text-node
    (car (css-selectors:query path node)))))

(defun sort-hash-keys (hashtbl &key (exclude-keys nil))
  (let ((ks nil))
    (loop for k being the hash-keys of hashtbl do
      (unless (member k exclude-keys :test 'equal)
        (setf ks (cons k ks))))
    (sort ks #'string<)))

(defun nonempty-kv-pairs (ht ks)
  (let ((ps nil))
    (dolist (k (reverse ks))
      (let ((v (gethash k ht)))
        (unless (or (null v) (string= (wz:tostr v) ""))
          (setf ps (cons (list k (gethash k ht)) ps)))))
    ps))

(defun get-cdata (dom-root path)
  (dom:node-value (dom:first-child (car (css-selectors:query path dom-root)))))

(defun make-qrcode (text save-to-filename
                    &key (back-color "ffffff") (fore-color "000000") (error-correction "h")
                      (size 256) (margin 16) (logo nil))
  (let* ((encoded-text (do-urlencode:urlencode text))
         (encoded-logo (do-urlencode:urlencode logo))
         (url (format nil "http://qr.liantu.com/api.php?text=~a&bg=~a&fg=~a&el=~a&w=~a&m=~a&logo=~a"
                      encoded-text back-color fore-color error-correction size margin encoded-logo))
         (imgdata (drakma:http-request url)))
    (with-open-file (ofs save-to-filename :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (write-sequence imgdata ofs))
    save-to-filename))

(defun plist_to_hashtable (plist)
  "convert plist to hashtable and hyphen in keys to underscore."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on plist by #'cddr do
      (wz:seth ht (cl-ppcre:regex-replace-all "-" (string-downcase (symbol-name k)) "_")
               (if (and (keywordp v) (equal v :null))
                   nil
                   v)))
    ht))

(defun readlines (fname)
  (with-open-file (ist fname)
    (let ((ls nil))
      (do ((s (read-line ist)
              (read-line ist nil 'eof)))
          ((eq s 'eof) ls)
        (push s ls))
      (nreverse ls))))


(defun treep (x)
  (or (null x)
      (and (consp x)
           (loop for e in x thereis (consp e)))))

(defun ordered-unique (lst)
  (let ((c nil))
    (dolist (e lst)
      (unless (member e c)
        (push e c)))
    (reverse c)))

(defun ordered-union (a b)
  (let ((ua (ordered-unique a))
        (ub (ordered-unique b)))
    (append ua (remove-if (lambda (e) (member e ua)) ub))))

(defun occurrences (lst)
  (let ((d (mapcar (lambda (e) (cons e 0))
                   (unique lst))))
    (mapcar (lambda (e)
              (incf (cdr (assoc e d))))
            lst)
    d))

(defun posadd-rec (lst &optional (i 0))
  (if lst
      (cons (+ (car lst) i)
            (posadd-rec (cdr lst) (1+ i)))))

(defun posadd-iter (lst)
  (do ((i 0 (1+ i))
       (a lst (cdr a)))
      ((null a) lst)
    (incf (car a) i)))

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

(defun prins (&rest args)
  (format t "~&~{~a~^ ~}~%" args))

(defun mkrng (n)
  (lambda ()
    (random n)))

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

;; bound should be integer, float, long-float
(defun gen-random-vector (n &key (bound 1.0) (rng *random-state*))
  (let ((a (make-array n)))
    (dotimes (i n)
      (setf (aref a i) (random bound rng)))
    a))

(defun gen-sequence-vector (n)
  (let ((a (make-array n)))
    (dotimes (i n)
      (setf (aref a i) i))
    a))

(defun shuffle-vector (a &optional (b 0) (e (length a)))
  (let ((d (- e b)))
    (loop for i from b to (1- e) do
      (rotatef (aref a i) (aref a (+ b (random d)))))
    a))

(defun gen-shuffle-vector (n)
  (shuffle-vector (gen-sequence-vector n)))

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
  (let* ((a (gen-random-vector n :bound 100))
         (v (list (sum-array-v0 a)
                  (sum-array-v1 a)
                  (sum-array-v2 a)
                  (sum-array-v3 a))))
    (unless (apply #'= v)
      (format t "~s: ~s~%" a v)
      (return-from test-sum-array nil))
    t))


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

(defun bubble-sort-v2 (a n)
  (dotimes (j (- n 1))
    (dotimes (i (- n 1 j))
      (if (< (aref a i) (aref a (1+ i)))
          (rotatef (aref a i) (aref a (1+ i)))))))

(defun sort-stable-demo ()
  (let ((a (map 'vector #'cons
                (gen-random-vector 10 :bound 5)
                (gen-random-vector 10 :bound 100))))
    (flet ((cmp (x y) (< (car x) (car y))))
      (print (sort a #'cmp))
      (print (sort a #'cmp))
      (print (stable-sort a #'cmp))
      (print (stable-sort a #'cmp))))
  nil)

(defstruct vec3f
  (x 0.0 :type float)
  (y 0.0 :type float)
  (z 0.0 :type float))

(defun vec3f-zero (v)
  (setf (vec3f-x v) 0.0
        (vec3f-y v) 0.0
        (vec3f-z v) 0.0))

(defun vec3f-unitx () (make-vec3f :x 1.0))
(defun vec3f-unity () (make-vec3f :y 1.0))
(defun vec3f-unitz () (make-vec3f :z 1.0))

(defun vec3f-dot (va vb)
  (+ (* (vec3f-x va) (vec3f-x vb))
     (* (vec3f-y va) (vec3f-y vb))
     (* (vec3f-z va) (vec3f-z vb))))

(defun vec3f-cross (va vb)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let ((ax (vec3f-x va))
        (ay (vec3f-y va))
        (az (vec3f-z va))
        (bx (vec3f-x vb))
        (by (vec3f-y vb))
        (bz (vec3f-z vb)))
    (make-vec3f :x (- (* ay bz) (* az by))
                :y (- (* az bx) (* ax bz))
                :z (- (* ax by) (* ay bx)))))

(defun vec3f-test (n)
  ;; CL:C++ ~= 17
  (let ((va (vec3f-unitx))
        (vb (vec3f-unity))
        (vt))
    (dotimes (i n)
      (setf vt (vec3f-cross va vb))
      (setf va vb vb vt))
    va))

(defun q1 ()
  (do ((i 0 (1+ i)))
      ((and (integerp (sqrt (+ i 100)))
            (integerp (sqrt (+ i 268))))
       i)))

(defun read-lines (ifs &optional (lines nil))
  (let ((s (read-line ifs nil)))
    (if s
        (read-lines ifs (cons s lines))
        (nreverse lines))))

(defmacro backwards (expr)
  (reverse expr))

(defun is-macro (sym)
  (macro-function sym))

(defun primep (n)
  (and (> n 1)
       (loop for i from 2 to (isqrt n)
             never (zerop (mod n i)))
       n))

(defun next-prime (n)
  (loop for i from n when (primep i) return i))

(defun loop-demo ()
  (loop for i in '(1 2 3 4 5)
        minimizing i into min
        maximizing i into max
        summing i into total
        counting (evenp i) into evens
        counting (oddp i) into odds
        finally (return (list min max total evens odds))))

(defmacro with-gensym ((&rest names) &body body)
  `(let ,(loop for nm in names collect `(,nm (gensym)))
     ,@body))

(defmacro doprimes ((var from to) &body body)
  (let ((evnm (gensym)))
    `(do ((,var (next-prime ,from) (next-prime (1+ ,var)))
          (,evnm ,to))
         ((> ,var ,evnm))
       ,@body)))

(defvar *test-name* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check-v1 (form)
  `(report-result ,form ',form))

(defmacro combine-results (&body forms)
  (with-gensym (result)
    `(let ((,result t))
       ,@(loop for f in forms collect
               `(unless ,f (setf ,result nil)))
     ,result)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro deftest (name params &body body)
  `(defun ,name ,params
     (let ((*test-name* ',name))
       ,@body)))

(deftest test-add ()
  (check
    (= (+) 0)
    (= (+ 1 2) 3)))

(deftest test-mul ()
  (check
    (= (*) 1)
    (= (* 2 3) 6)))

(defun puts (&rest args)
  (dolist (e args)
    (format t "~a " e))
  (format t "~%"))

(defun component-present-p (val)
  (and val
       (not (eql val :unspecific))))

(defun dir-pathname-p (p)
  (and (not (component-present-p (pathname-name p)))
       (not (component-present-p (pathname-type p)))
       p))

(defun pathname-as-dir (name)
  (let ((pname (pathname name)))
    (when (wild-pathname-p pname)
      (error "can't reliably convert wild pathnames."))
    (if (not (dir-pathname-p name))
        (make-pathname :directory (append (or (pathname-directory pname) (list :relative))
                                          (list (file-namestring pname)))
                       :name nil
                       :type nil
                       :defaults pname)
        pname)))

(defun gen-big-lisp (filename &key (n 655360))
  (with-open-file (s filename :direction :output :if-exists :supersede)
    ;(format s "(defvar *a* 0)~%")
    (dotimes (i n)
      (format s "(setf *a* '(a b c d e ~a))~%" i))))

(defclass user ()
  ((instance-count :allocation :class :initform 0)
   (username :initarg :username)
   (password :initarg :password)))

(define-condition zerr (error)
  ((text :initarg :text :reader text)))

(defun restart-test1 ()
  (loop for i from 2 downto -2
        for x = (restart-case (/ i)
                  (skip-zero ()
                    (format t "skip: ~s~%" i)
                    nil))
        when x collect it))

(defun skip-zero (x)
  (declare (ignore x))
  (let ((sz (find-restart 'skip-zero)))
    (when sz (invoke-restart 'skip-zero))
    (format t "restart 'skip-zero not found.")))

(defun restart-test2 ()
  (handler-bind
      ((division-by-zero
         (lambda (x)
           (declare (ignore x))
           (invoke-restart 'skip-zero))))
    (restart-test1)))

(defun restart-test3 (&optional (fn #'restart-test1))
  (handler-bind
      ((division-by-zero #'skip-zero))
    (funcall fn)))

(defun prins (&rest args)
  (format t "~&~{~s~^ ~}~%" args))

(defun get-sqrt2 ()
  (let ((x (sqrt 2)))
    ;;(prins x)
    x))

(defun load-time-test ()
  (load-time-value (get-sqrt2)))

;;;; ANSI Common Lisp
(defun unique (lst)
  (let ((c nil))
    (dolist (e lst)
      (unless (member e c)
        (push e c)))
    (reverse c)))

(defun new-union (a b)
  (let ((ua (unique a))
        (ub (unique b)))
    (append ua (remove-if (lambda (e) (member e ua)) ub))))

(defun count-elements (lst)
  (let ((d (mapcar (lambda (e) (cons e 0))
                   (unique lst))))
    (mapcar (lambda (e)
              (incf (cdr (assoc e d))))
            lst)
    d))

;; from "On Lisp"
(defun last1 (lst)
  (car (last lst)))

(defun singlep (lst)
  (and (consp lst)
       (null (cdr lst))))

(defun append1 (lst x)
  (append lst (list x)))

(defun nappend1 (lst x)
  (nconc lst (list x)))

(defun mklist (x)
  (if (listp x) x (list x)))

(defun longerp (a b)
  (labels ((cmp (x y)
             (and (consp x)
                  (or (null y)
                      (cmp (cdr x) (cdr y))))))
    (if (and (listp a) (listp b))
        (cmp a b)
        (> (length a) (length b)))))

(defun filter (fn lst)
  (let (r)
    (dolist (e lst)
      (let ((v (funcall fn e)))
        (if v (push v r))))
    (nreverse r)))

(defun filter-v1 (fn lst)
  (loop for e in lst when (funcall fn e) collect it))

(defun group (lst n)
  (assert (plusp n)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x)
                           (rec (cdr x) acc))))))
    (rec x nil)))

(defun flatten-v1 (lst)
  (if (consp lst)
      (append (flatten-v1 (car lst))
              (flatten-v1 (cdr lst)))
      (if lst
          (list lst)
          nil)))

(defun flatten-v2 (lst &optional rst)
  (cond ((consp lst)
         (flatten-v2 (car lst)
                     (if (cdr lst)
                         (flatten-v2 (cdr lst) rst)
                         rst)))
        (lst (cons lst rst))
        (t rst)))

(defun nest-seq (n)
  (do ((i 0 (1+ i))
       (r nil ))
      ((>= i n) r)
    (setf r (if r
                (cons r (cons i nil))
                (cons i nil)))))

(defun test-flatten (n)
  (let ((a (nest-seq n)))
    (mapcar (lambda (fn)
              (time (length (funcall fn a))))
            (list #'flatten #'flatten-v1 #'flatten-v2))))

(defun remove-node-if (test x &optional acc)
  (cond ((null x) acc)
        ((atom x) (if (funcall test x) acc (cons x acc)))
        ((consp (car x)) (if (funcall test (car x))
                             acc
                             (cons x acc)))
        (t (let ((ra (remove-node-if test (car x)))
                 (rd (remove-node-if test (cdr x) acc)))
             ))))

(defun count-common (a b)
  (length (intersection a b)))


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

(defun find-nested (lst)
  (find-if #'consp lst))

(defun beforep (a b lst)
  (member b (member a lst)))

(defun half (x)
  (* x 0.5))

(defun avg (&rest numbers)
  (/ (apply #'+ numbers)
     (length numbers)))


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

(defun ffo-with-do (x)
  (do ((z x (rest z))
       (e (first x) (first z))
       (dummy (format t "z: ~a, e: ~a~%" x (first x)) (format t "z: ~a, e: ~a~%" (rest z) (first z))))
      ((null z) nil)
    (if (oddp e) (return e))))

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

(defun listprefixp (a b)
  (do ((pa a (cdr pa))
       (pb b (cdr pb)))
      ((null pa) t)
    (unless (eq (car pa) (car pb))
        (return nil))))

(defun listappearsp (a b)
  (do ((pb b (cdr pb)))
      ((null pb) (null a))
    (if (listprefixp a pb)
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
    (unless (listprefixp a pb)
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


(defun sort-unique-symbols (lists)
  (sort (remove-duplicates (apply #'append lists))
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
                 (gen-combinations (sort-unique-symbols graph) 3)))
