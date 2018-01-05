#lang sicp

(#%require (file "util.rkt"))

;ex. 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ this-coeff
        (* higher-terms x)))
   0
   coefficient-sequence))

