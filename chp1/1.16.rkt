#lang sicp
(#%require (file "util.rkt"))
; ex 1.16
(define (fast-expt-it b n)
    (define (fast-expt-it-aux b n a)
      (cond ((= n 0) a)
            ((even? n) (fast-expt-it-aux (square b) (/ n 2) a))
            (else (fast-expt-it-aux b (- n 1) (* a b)))))
    (fast-expt-it-aux b n 1))
