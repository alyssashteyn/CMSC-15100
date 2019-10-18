#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(define-struct Date
  ([m : Integer]
   [d : Integer]
   [y : Integer]))

(define-type Day
  (U 'Su 'M 'Tu 'W 'Th 'F 'Sa))

(: leap?(Integer -> Boolean))
;whether a year is a leap year or not
(define (leap? year)
  (cond
    [(= (remainder year 400) 0) #t]
    [(and (= (remainder year 4) 0) (= (remainder year 100) 0)) #f]
    [(and (= (remainder year 4) 0) #t)]
         [else #f]))

(check-expect (leap? 2002) #f)
(check-expect (leap? 2000) #t)

(: date=? (Date Date -> Boolean))
;test whether two dates are exactly the same
(define (date=? d1 d2)
  (and (= (Date-m d1) (Date-m d2))
       (= (Date-d d1) (Date-d d2))
       (= (Date-y d1) (Date-y d2))))

(check-expect (date=? (Date 1 1 2019) (Date 1 1 2020)) #f)
(check-expect (date=? (Date 1 7 2019) (Date 1 7 2019)) #t)
  
(: date<? (Date Date -> Boolean))
;test whether first occurs before the other
(define (date<? d1 d2)
  (cond
    [(< (Date-y d1) (Date-y d2)) #t]
    [(and (= (Date-y d1) (Date-y d2)) (< (Date-m d1) (Date-m d2)) #t)]
    [(and (= (Date-y d1) (Date-y d2)) (= (Date-m d1) (Date-m d2))
          (< (Date-d d1) (Date-d d2)) #t)]
    [(and (= (Date-y d1) (Date-y d2)) (= (Date-m d1) (Date-m d2))
          (= (Date-d d1) (Date-d d2)) #f)]
    [else #f]))

(check-expect (date<? (Date 1 1 2019) (Date 1 1 2020)) #t)
(check-expect (date<? (Date 12 1 2019) (Date 12 6 2019)) #t)
(check-expect (date<? (Date 12 1 2019) (Date 11 1 2019)) #f)
(check-expect (date<? (Date 12 6 2019) (Date 12 1 2020)) #t)

(: month-adj (-> Date Integer))
;values for months to be calculated in the day-of-week function
(define (month-adj d1)
  (cond
    [(= (Date-m d1) 1)
     (if (leap? (Date-y d1)) 0 1)]
    [(= (Date-m d1) 2)
     (if (leap? (Date-y d1)) 4 3)]
    [(= (Date-m d1) 3) 4]
    [(= (Date-m d1) 4) 0]
    [(= (Date-m d1) 5) 2]
    [(= (Date-m d1) 6) 5]
    [(= (Date-m d1) 7) 0]
    [(= (Date-m d1) 8) 3]
    [(= (Date-m d1) 9) 6]
    [(= (Date-m d1) 10) 1]
    [(= (Date-m d1) 11) 4]
    [(= (Date-m d1) 12) 6]
    [else -1]))

(check-expect (month-adj (Date 12 07 1999)) 6)
(check-expect (month-adj (Date 10 16 2019)) 1)
(check-expect (month-adj (Date 1 1 2020)) 0)

(: day-of-week (Date -> Day))
;calculate which day of the week it is
(define (day-of-week d1)
  (cond
    [(= 0 (remainder (+ (- (Date-y d1) 1900) (month-adj d1)
                        (Date-d d1) (exact-floor (/ (Date-y d1) 4))) 7)) 'Su]
    [(= 1 (remainder (+ (- (Date-y d1) 1900) (month-adj d1)
                        (Date-d d1) (exact-floor (/ (Date-y d1) 4))) 7)) 'M]
    [(= 2 (remainder (+ (- (Date-y d1) 1900) (month-adj d1)
                        (Date-d d1) (exact-floor (/ (Date-y d1) 4))) 7))'Tu]
    [(= 3 (remainder (+ (- (Date-y d1) 1900) (month-adj d1)
                        (Date-d d1) (exact-floor (/ (Date-y d1) 4))) 7)) 'W]
    [(= 4 (remainder (+ (- (Date-y d1) 1900) (month-adj d1)
                        (Date-d d1) (exact-floor (/ (Date-y d1) 4))) 7))'Th]
    [(= 5 (remainder (+ (- (Date-y d1) 1900) (month-adj d1)
                        (Date-d d1) (exact-floor (/ (Date-y d1) 4))) 7))'F]
    [else 'Sa]))

(check-expect (day-of-week (Date 10 14 2019)) 'M)
(check-expect (day-of-week (Date 10 15 2019)) 'Tu)
(check-expect (day-of-week (Date 10 16 2019)) 'W)
(check-expect (day-of-week (Date 10 17 2019)) 'Th)
(check-expect (day-of-week (Date 10 18 2019)) 'F)
(check-expect (day-of-week (Date 10 19 2019)) 'Sa)
(check-expect (day-of-week (Date 10 20 2019)) 'Su)         
(check-expect (day-of-week (Date 1 10 2019)) 'Th)
(check-expect (day-of-week (Date 12 7 1999)) 'Tu)


(: smart-date (Integer Integer Integer -> Date))
;check whether a date is valid or not
(define (smart-date m d y)
  (cond
    [(or (< m 1) (> m 12) (< d 1)) (error "day out of range")]
    [(and (or (= m 1) (= m 3) (= m 5) (= m 7) (= m 8) (= m 10)
              (= m 12)) (> d 31)) (error "day out of range")]
    [(and (or (= m 4) (= m 6) (= m 11)) (> d 30)) (error "day out of range")]
    [(and (leap? y) (= m 2) (> d 29)) (error "day out of range")]
    [(and (not (leap? y)) (= m 2) (> d 28)) (error "day out of range")]
    [else (Date m d y)]))
      
(check-error (smart-date 99 2 2019) "day out of range")
(check-error (smart-date 1 99 2019) "day out of range")
(check-error (smart-date 2 30 2020) "day out of range")
(check-error (smart-date 2 30 2021) "day out of range")
(check-expect (smart-date 10 12 2019) (Date 10 12 2019))
(check-expect (smart-date 1 1 2020) (Date 1 1 2020))
(check-expect (smart-date 2 29 2000) (Date 2 29 2000))


(: yesterday (Date -> Date))
;calculate yesterday
(define (yesterday d1)
  (cond
    [(and (= (Date-d d1) 1) (= (Date-m d1) 3)
          (leap? (Date-y d1))) (Date 2 29 (Date-y d1))]
    [(and (= (Date-d d1) 29) (= (Date-m d1) 2)
          (leap? (Date-y d1))) (Date 2 28 (Date-y d1))]
    [(and (= (Date-d d1) 1) (= (Date-m d1) 1)) (Date 12 31 (- (Date-y d1) 1))]
    [(and (= (Date-d d1) 1) (= (Date-m d1) 2)) (Date 1 31 (Date-y d1))]
    [(and (= (Date-d d1) 1) (= (Date-m d1) 3)) (Date 2 28 (Date-y d1))]
    [(and (= (Date-d d1) 1) (= (Date-m d1) 4)) (Date 2 28 (Date-y d1))]
    [(and (= (Date-d d1) 1) (= (Date-m d1) 5)) (Date 4 30 (Date-y d1))]
    [(and (= (Date-d d1) 1) (= (Date-m d1) 6)) (Date 5 31 (Date-y d1))]
    [(and (= (Date-d d1) 1) (= (Date-m d1) 7)) (Date 6 30 (Date-y d1))]
    [(and (= (Date-d d1) 1) (= (Date-m d1) 8)) (Date 7 31 (Date-y d1))]
    [(and (= (Date-d d1) 1) (= (Date-m d1) 9)) (Date 8 31 (Date-y d1))]
    [(and (= (Date-d d1) 1) (= (Date-m d1) 10)) (Date 9 30 (Date-y d1))]
    [(and (= (Date-d d1) 1) (= (Date-m d1) 11)) (Date 10 31 (Date-y d1))]
    [(and (= (Date-d d1) 1) (= (Date-m d1) 12)) (Date 11 30 (Date-y d1))]
    [else (Date (Date-m d1) (- (Date-d d1) 1) (Date-y d1))]))

(check-expect (yesterday (Date 1 10 2019)) (Date 1 9 2019))
(check-expect (yesterday (Date 2 10 2019)) (Date 2 9 2019))
(check-expect (yesterday (Date 1 1 2019)) (Date 12 31 2018))
(check-expect (yesterday (Date 2 29 2020)) (Date 2 28 2020))
(check-expect (yesterday (Date 3 1 2020)) (Date 2 29 2020))
               
(: tomorrow (Date -> Date))
;calculate the date after the date given
(define (tomorrow d1)
  (cond
    [(and (= (Date-d d1) 28) (= (Date-m d1) 2)
          (leap? (Date-y d1))) (Date 2 29 (Date-y d1))]
    [(and (= (Date-d d1) 29) (= (Date-m d1) 2)
          (leap? (Date-y d1))) (Date 3 1 (Date-y d1))]
    ;[(and (= (Date-m d1) 2) (= (Date-d d1) 28) (<= (Date-d d1) 29)
          ;(if (leap? (Date-y d1)) (Date 2 29 (Date-y d1)) (Date 3 1 (Date-y d1))))]
    [(and (= (Date-d d1) 31) (= (Date-m d1) 12)) (Date 1 1 (+ (Date-y d1) 1))]
    [(and (= (Date-d d1) 31) (= (Date-m d1) 1)) (Date 2 1 (Date-y d1))]
    [(and (= (Date-d d1) 28) (= (Date-m d1) 2)) (Date 3 1 (Date-y d1))]
    [(and (= (Date-d d1) 31) (= (Date-m d1) 3)) (Date 4 1 (Date-y d1))]
    [(and (= (Date-d d1) 30) (= (Date-m d1) 4)) (Date 5 1 (Date-y d1))]
    [(and (= (Date-d d1) 31) (= (Date-m d1) 5)) (Date 6 1 (Date-y d1))]
    [(and (= (Date-d d1) 30) (= (Date-m d1) 6)) (Date 7 1 (Date-y d1))]
    [(and (= (Date-d d1) 31) (= (Date-m d1) 7)) (Date 8 1 (Date-y d1))]
    [(and (= (Date-d d1) 31) (= (Date-m d1) 8)) (Date 9 1 (Date-y d1))]
    [(and (= (Date-d d1) 30) (= (Date-m d1) 9)) (Date 10 1 (Date-y d1))]
    [(and (= (Date-d d1) 31) (= (Date-m d1) 10)) (Date 11 1 (Date-y d1))]
    [(and (= (Date-d d1) 30) (= (Date-m d1) 11)) (Date 12 1 (Date-y d1))]
    [else (Date (Date-m d1) (+ (Date-d d1) 1) (Date-y d1))]))
        
(check-expect (tomorrow (Date 12 31 1999)) (Date 1 1 2000))
(check-expect (tomorrow (Date 1 31 2019)) (Date 2 1 2019))
(check-expect (tomorrow (Date 3 31 1969)) (Date 4 1 1969))
(check-expect (tomorrow (Date 1 15 2019)) (Date 1 16 2019))
(check-expect (tomorrow (Date 2 28 2019)) (Date 3 1 2019))
(check-expect (tomorrow (Date 2 28 2020)) (Date 2 29 2020))
(check-expect (tomorrow (Date 2 29 2020)) (Date 3 1 2020))


(: add-days (Integer Date -> Date))
;add an arbitrary amount of days to a given date and still return a valid date
(define (add-days add d1)
  (cond
    [(zero? add) d1]
    [(> add 0) (add-days (- add 1) (tomorrow d1))]
    [(< add 0) (add-days (+ add 1) (yesterday d1))]
    [else ('error)]))

(check-expect (add-days 7 (Date 10 25 2019)) (Date 11 1 2019))
(check-expect (add-days -7 (Date 10 25 2019)) (Date 10 18 2019))
(check-expect (add-days -27 (Date 10 25 2019)) (Date 9 28 2019))
(check-expect (add-days 15 (Date 2 28 2019)) (Date 3 15 2019))
(check-expect (add-days 15 (Date 2 28 2020)) (Date 3 14 2020))
(check-expect (add-days 30 (Date 12 07 1999)) (Date 1 6 2000))
(check-expect (add-days -30 (Date 1 6 2019)) (Date 12 7 2018))

(test)