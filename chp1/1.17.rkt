#lang sicp

(#%require (file "util.rkt"))

; ex. 1.17
(define (fast-mult a b)
    (define (double n) (+ n n))
    (define (halve n) (/ n 2))
    (cond ((= b 0) 0)
          ((even? b) (fast-mult (double a) (halve b)))
          (else (+ a (fast-mult a (- b 1))))))
