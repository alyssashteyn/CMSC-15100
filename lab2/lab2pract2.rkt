
#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(define-struct Date
  ([m : Integer]
   [d : Integer]
   [y : Integer]))

(define-type Day
  (U 'Su 'M 'Tu 'W 'Th 'F 'Sa))
----------------------------------------------------------------
(: leap?(Integer -> Boolean))
(define (leap? year)
  (or
   ((= (remainder year 400) 0) #t)
   (= (remainder year 4) 0)
   (cond
     [(= (remainder year 100) 0) #f #t])))

(check-expect leap? 2002) #f)
(check-expect leap? 2020) #t)