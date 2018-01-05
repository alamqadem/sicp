#lang sicp

(#%require (file "util.rkt"))

; ex. 1.18
(define (fast-mult-it a b)
    (define (double n) (+ n n))
    (define (halve n) (/ n 2))
    (define (fast-mult-it-aux a b c)
    (cond ((= b 0) c)
          ((even? b) (fast-mult-it-aux (double a) (halve b) c))
          (else (fast-mult-it-aux a (- b 1) (+ a c)))))
    (fast-mult-it-aux a b 0))

