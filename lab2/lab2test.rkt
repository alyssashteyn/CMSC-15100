#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

;;(: leap?(Integer -> Boolean))
;;(define (leap? year)
;;  (or
;;   ((= (remainder year 400) 0))
;;  (and (= (remainder year 4) 0) (= (remainder year 100) 0))))
;;
;;(check-expect leap? 2002) #f
;;(check-expect leap? 2020) #t
;
;(: smart-date (Integer Integer Integer -> Date))
;(define smart-date (m d y)
;  ;(cond
;    [(or (< m 1) (> m 12)) (raise error)]
;  (check-expect (smart-date 0 9 2019) )
;
;
;;    (and [or (= m 1) (= m 2) (= m 3) (= m 4) (= m 5) (= m 6) (= m 7)
;;             (= m 8) (= m 9) (= m 10) (= m 11) (= m 12)] (< d 1))
;;    (error "day out of range"))
;;  (and (= m 1) (= m 3) (= m 5) (= m 7) (= m 8) (= m 10)
;;       (= m 12) (> d 31) (error "day out of range"))
;;  (and (= m 2) (> d 28) (error "day out of range")) ;; CHECK LEAP YEAR
;;  (and (= m 4) (= m 6) (= m 9) (= m 11) (> d 30) (error "day out of range"))
;  ;(else -1)))
;
;(check-error (smart-date 0 9 2019) )

(test)