#lang sicp

;ex. 2.8
(#%require (file "util.rkt"))

(define (sub-interval x y)
  (make-interval
   (- (lower-bound x) (upper-bound y))
   (- (upper-bound x) (lower-bound y))))
