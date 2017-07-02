;;;; CL-RUSH

;;; 这篇速成记录了我在学习Lisp时最想了解的一些东西，目标读者是用过一些编程语言、想尽快用Lisp做些实用程序的开发者。

#|
multiline-comments
  #| nestable |#
|#

                                        ; line-comments

;;;; title
;;; intro
;; state
                                        ;explanation 从分号到行尾是注释。
                                        ; continuation

;; number

;; integer
;; 大小无限（或说只受你的存储限制），形如 123 -456
;; 支持2~36进制，形如 #nnRdddd 其中nn为基，R为固定的分隔，后续"数字"。
1

;; rational CL支持分数，这在其他语言中不太常见。
2/3

;; float
3.14

;; long-float
1.234d9

;; complex
#c(2.0 3.5)

;; symbol：符号。单引号表示引用
'foo 'BAR 'a-2_#

;; 标识符"一般"不区分大小写，例如
nil nIL Nil NIL

;; char: 字符
#\a #\A #\0 #\\ #\' #\"

;; 对于C中的转义字符，CL有特殊名字，例如
#\Newline #\Tab #\Return #\Nul

;; string: 区分大小写
"abc-XYZ-123"
"字符串"

;; 常量
(defconstant gravity 9.8)

;; 全局变量
(defvar error-count 0)

(defparameter show-error t)

;; 函数调用
(cos pi)

;; 赋值
(defvar foo 0)
(setf foo 1)
(incf foo)
(decf foo 2)

;; 算术运算
;; 关系运算
;; 逻辑运算
;; 函数定义
;; 局部变量
;; 顺序
;; 选择
;; 循环
;; 命名空间
;; 数据类型
;; 字符串
;; 向量
;; 列表
;; 关联列表
;; 哈希表
;; 日期与时间
;; 文件操作
;; 数据库
;; 网络

(defun type-convert-demo ()
  (list (float 1/3)
        (float 2/3 0d0)
        (format nil "~s" 123)))

(coerce vec 'list) ; 向量（数组）转列表

(coerce lst 'vector) ; 列表转向量（数组）

(flexi-streams:string-to-octets s) ; 字符串转字节数组
