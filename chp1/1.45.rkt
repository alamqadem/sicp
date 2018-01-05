#lang sicp
(#%require (file "util.rkt") (file "1.43.rkt"))
;ex. 1.45
(define (root n x)
  (fixed-point-of-transform
   (lambda (y) (/ x (fast-expt y (- n 1))))
   (repeated average-damp (- n 2))
   1.0))
