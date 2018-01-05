#lang sicp

(#%require (file "util.rkt"))
; square root
(define (sqrt x)

  (define (good-enough? guess)
    (< (abs (- (/ (square guess) x) 1.0)) 0.1))
    
  (define (improve guess)
    (average guess (/ x guess)))

  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))

  (sqrt-iter 1.0))
