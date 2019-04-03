(ql:quickload :postmodern)
(postmodern:disconnect-toplevel)
(postmodern:connect-toplevel "dbeib" "useib" "test" "localhost")

(defparameter *city-country*
  '(成都 中国
    广州 中国
    柏林 德国
    名古屋 日本
    中国 中国
    德国 德国
    日本 日本))

(defparameter *country-continent*
  '(中国 亚洲
    德国 欧洲
    日本 亚洲
    亚洲 亚洲
    欧洲 欧洲))

(format t "~a rows~%"
        (postmodern:query "select count(*) as rows from tmp.travel a" :single))

(print (postmodern:query "
select  customer, city, to_char(travel_date, 'yyyy-mm-dd') as dt
from    tmp.travel a
limit   10" :plists))

(defun stat (dest &optional (limit 100))
  (print (count dest
                (mapcar (lambda (r) (getf *city-country* (intern (getf r :city))))
                        (postmodern:query "
select  customer, city, to_char(travel_date, 'yyyy-mm-dd') as dt
from    tmp.travel a
limit $1
" limit :plists)))))

(time (stat '日本 10000000))
