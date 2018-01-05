#lang sicp
(#%provide compose)
;ex. 1.42
(define (compose f g)
    (lambda (x) (f (g x))))
