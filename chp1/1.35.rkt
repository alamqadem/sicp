#lang sicp

(#%require (file "util.rkt"))

;ex. 1.35
(fixed-point (lambda (x) (+ 1 (/ 1 x)))
             1.0)
