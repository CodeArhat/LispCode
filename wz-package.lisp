(in-package :common-lisp-user)

(defpackage com.zlisp.wz
  (:nicknames :wz)
  (:use :common-lisp)
  (:export

   ;; general
   :last-elt
   :singlep

   ;; math
   :ackermann
   :square
   :cube
   :cbrt

   :nvl :nvls
   :empty-to-null

   :zerror
   :raise
   :ecode
   :einfo

   :toupper :tolower
   :trim :ltrim :rtrim
   :strcat :split :replace-all
   :utf8decode
   :utf8encode
   :utf8read
   :jsondecode :jsonencode
   :itos :stoi :tostr

   :now :unixtime
   :ymdhms
   :ymdhms-num
   :yymmdd :yyyymmdd :yyyy-mm-dd
   :iso-tms-ms :iso-tms-us

   :shl :shr :rotl :rotr

   :randnum
   :randnum4
   :randnum6
   :randnum8
   :randbit32
   :randbs62

   :md5s
   :md5su
   :sha1s
   :sha1su
   :sha256s
   :sha256su
   :sha3s
   :sha3su

   :password-hash
   :password-check

   :geth
   :seth
   :dump

   :pkcs7padding
   :pkcs7trim
   :aes-encrypt-blocks
   :aes-decrypt-blocks
   :aes-encrypt-bytes
   :aes-decrypt-bytes
   :aes-encrypt-str
   :aes-decrypt-str

   :xml-dom
   :dom-node-value
   :sort-hash-keys
   :nonempty-kv-pairs
   :get-cdata

   :make-qrcode
   :readlines
   :plist_to_hashtable
   :accum-squares))
