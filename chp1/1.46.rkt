#lang sicp

(#%require (file "util.rkt"))
;ex. 1.46
 (define (iterative-improve good-enough? improve)
    (define f
             (lambda (guess)
               (if (good-enough? guess)
                   guess
                   (f (improve guess)))))
      f)

 (define (sqrt-iter guess x)
    (define (good-enough? guess)
      (< (abs (- (square guess) x)) 0.00001))
    (define (improve guess)
      (average guess (/ x guess)))
    ((iterative-improve good-enough?
                        improve)
     guess))

(define (fixed-point f first-guess)
  (define (good-enough? x)
    (< (abs (- x (f x))) tolerance))
  (define (improve x)
    (f x))
  ((iterative-improve good-enough?
                      improve)
   first-guess))

