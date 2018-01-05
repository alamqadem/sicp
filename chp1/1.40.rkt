#lang sicp
(#%require (file "util.rkt"))
; ex. 1.40
(define (cubic a b c)
    (lambda (x) (+ (cube x)
                   (* a (square x))
                   (* b x)
                   c)))
