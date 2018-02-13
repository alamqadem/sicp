#lang sicp

(#%require (file "../util.rkt"))

;ex. 2.48
 (define (make-segment start end)
    (cons start end))
 (define (start-segment s)
    (car s))
 (define (end-segment s)
    (cdr s))
