#lang sicp

(#%require (file "../util.rkt"))
(#%require (file "constraints-propagation.rkt"))
; ex. 3.33

(define (averager a b c)
    (let ((u (make-connector))
          (v (make-connector)))
        (adder a b u)
        (multiplier c v u)
        (constant 2 v)
        'ok))

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(averager A B C)

(probe "A" A)
(probe "B" B)
(probe "C" C)

(set-value! A 2 'user)
(set-value! B 2 'user)

(forget-value! A 'user)
(set-value! C 4 'user)