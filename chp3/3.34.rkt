#lang sicp

(#%require (file "../util.rkt") (file "constraints-propagation.rkt"))
; ex. 3.34

; the flaw is that given B there is no way to compute A in terms
; of the basic constraints 
; the multiplier it is not aware of the relation m1 = m2
(define (squarer a b) (multiplier a a b))

(define A (make-connector))
(define B (make-connector))
(squarer A B)

(probe "A" A)
(probe "B" B)

(set-value! A 2 'user)

(forget-value! A 'user)
(set-value! B 9 'user)