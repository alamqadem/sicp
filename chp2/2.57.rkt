#lang sicp

(#%require (file "../util.rkt") (file "deriv.rkt"))

;ex. 2.57
(define (make-sum a1 a2 . a-rest)
  (append (list '+ a1 a2) a-rest))

(define (make-product m1 m2 . m-rest)
  (append (list '* m1 m2) m-rest))

(define (addend s) (cadr s))

(define (augend s) (apply make-sum (cddr s)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (apply make-product (cddr p)))

