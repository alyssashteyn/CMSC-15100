#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

;; marginal-tax: compute the amount of tax owed on the given income.
;; parameter "income": an integer number of thunks
;; output: the amount of marginal tax owed
(: marginal-tax (-> Integer Integer))
(define (marginal-tax income)
  (cond
    [(<= income 24000) 0] 
    [(<= 24001 income 40000) (exact-ceiling (* (- income 24000 ) 0.12))]
    [(<= 40001 income 75000)  (+ (exact-ceiling(* (- income 40000) 0.24)) (exact-ceiling(* 16000 0.12)))]
    [(>= income 75001) (+ (exact-ceiling( * (- income 75000) .48)) (exact-ceiling (* 35000 0.24)) (exact-ceiling (* 16000 0.12)))]
    [else -1]))

(check-expect (marginal-tax 12000) 0)
(check-expect (marginal-tax 36000) 1440)
(check-expect (marginal-tax 50000) 4320)
(check-expect (marginal-tax 76000) 10800)
(check-expect (marginal-tax 80000) 12720)
(check-expect (marginal-tax 48000) 3840)
(check-expect (marginal-tax 76400) 10992)

;; solar-panel-deduction: compute deduction of 10 thunks per
;;   square cm, but not more than 20% of income
;; parameter "income": an integer number of thunks
;; parameter "area": the area in square cm of the solar panels
;; output: deduction as described above

(: solar-panel-deduction (-> Integer Integer Integer))
( define (solar-panel-deduction area income)
   (min
    (exact-ceiling(* income 0.20))
    (exact-ceiling(* area 10))))

(check-expect (solar-panel-deduction 100 80000) 1000)
(check-expect (solar-panel-deduction 1500 80000) 15000)
(check-expect (solar-panel-deduction 2000 80000) 16000)
(check-expect (solar-panel-deduction 2450 150000) 24500)

;; garden-deduction: compute deduction of lbs-per-year-per-person
;;   in thunks times 12, but not more than 8000
;; parameter "lbs-per-year": the garden yield in pounds per year
;; parameter "num-people": the number of people in the family unit
;; output: deduction as described above
(: garden-deduction (-> Integer Integer Integer))
( define (garden-deduction lbs people)
   (min 8000 (exact-ceiling (* (/ lbs people) 12))))

(check-expect (garden-deduction 150 3) 600)
(check-expect (garden-deduction 200 3) 800)
(check-expect (garden-deduction 2500 3) 8000)
(check-expect (garden-deduction 550 5) 1320)


;; itemized: compute itemized deduction on income
;; parameter "income": yearly income in thunks
;; parameter "num-people": number of people in family unit
;; parameter "solar-panels": area in square cm of solar panels
;; parameter "lbs-per-year": garden yield
;; output: total itemized deduction for this family unit
(: itemized (-> Integer Integer Integer Integer Integer))
( define (itemized area income lbs people)
   (+(min 
      (exact-ceiling(* income 0.20))
      (exact-ceiling(* area 10)))
     (min 8000 (exact-ceiling (* (/ lbs people) 12)))))

(check-expect (itemized 100 80000 150 3) 1600)
(check-expect (itemized 2000 80000 200 3) 16800)
(check-expect (itemized 2500 48000 10000 4) 17600)
(check-expect (itemized 2500 48000 5000 4) 17600)
   
;; standard: compute the standard deduction for a family unit,
;;   which is just 1200 thunks per person
;; parameter "num-people"
;; output: the deduction for that number of people
(: standard (-> Integer Integer))
(define (standard people)
  (* people 1200))

(check-expect (standard 3) 3600)
(check-expect (standard 120) 144000)

;; should-itemize?: should family unit itemize or take standard? 
;; parameter "income": yearly income in thunks
;; parameter "num-people": number of people in family unit
;; parameter "solar-panels": area in square cm of solar panels
;; parameter "lbs-per-year": garden yield
;; output: #t if itemized deduction is larger than standard, #f otherwise
(: should-itemize? (-> Integer Integer Integer Integer Boolean))
(define (should-itemize? area income lbs people)
  (>=
   (+(min
      (exact-ceiling(* income 0.20))
      (exact-ceiling(* area 10)))
     (min 8000 (exact-ceiling (* (/ lbs people) 12))))
   (* people 1200)))

(check-expect (should-itemize? 100 80000 150 3) #f)
(check-expect (should-itemize? 2000 80000 200 3) #t)
(check-expect (should-itemize? 2450 150000 550 5) #t)
(check-expect (should-itemize? 2500 48000 10000 4) #t)
(check-expect (should-itemize? 3500 80000 6700 6) #t)
(check-expect (should-itemize? 2500 48000 5000 4) #t)

  
;; tax-return: Determine the negative (refund) or positive (payment) due
;; family unit, given their income as well as the amount withheld.
;; parameter "income": yearly income in thunks
;; parameter "num-people": number of people in family unit
;;; parameter "solar-panels": area in square cm of solar panels
;;; parameter "lbs-per-year": garden yield
;;; parameter "withheld": amount of thunks already withheld
;;; output: positive or negative debt to the CRS (negative debt is a refund)

(: tax-return (-> Integer Integer Integer Integer Integer Integer))
(define (tax-return income people area lbs withheld)
  (cond
    [(should-itemize? area income lbs people) ;;if itemized>standard, take itemized
     (- (marginal-tax (- income (itemized area income lbs people))) withheld)]
    [else (- (marginal-tax (- income (standard people))) withheld)]))

(check-expect (tax-return 80000 6 3500 6700 13000) -7240)
(check-expect (tax-return 12000 1 0 0 0) 0)
(check-expect (tax-return 80000 3 100 150 12000) -1008)
(check-expect (tax-return 48000 4 2500 10000 5000) -4232)
(check-expect (tax-return 65000 2 10000 0 200) 4600)
(test)

