#lang sicp
(#%require (file "util.rkt"))

; cube root
(define (cbrt x)
  
  (define (good-enough? guess)
    (< (abs (- (/ (cube guess) x) 1.0)) 0.1))

  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (define (cbrt-iter guess)
    (if (good-enough? guess)
        guess
        (cbrt-iter (improve guess))))
  
  (cbrt-iter 1.0))
