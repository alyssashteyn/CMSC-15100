#lang typed/racket
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
;;drawing three pictures through Racket

(define mushroom
  (above
   (ellipse 60 30 "solid" "red")
   (rectangle 20 40 "solid" "brown")))

(define dot(circle 5 "solid" "white"))

(overlay/align "center" "top" dot mushroom)

;;(define lizard

(define pig
(below
  (circle 40 "solid" "red")
  (ellipse 30 15 "solid" "pink")))





