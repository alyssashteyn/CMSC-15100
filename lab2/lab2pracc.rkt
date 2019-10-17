#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(: leap?(Integer -> Boolean))
(define (leap? year)
  (cond
    [(= (remainder year 400) 0) #t]
    [(and (= (remainder year 4) 0) (= (remainder year 100) 0)) #f]
    [(and (= (remainder year 4) 0) #t)]
         [else #f]))

(check-expect (leap? 2002) #f)
(check-expect (leap? 2020) #t)

(test)