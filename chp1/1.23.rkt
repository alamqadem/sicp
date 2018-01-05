#lang sicp

(#%require (file "util.rkt"))

;ex. 1.23
(define (find-divisor n test-divisor)
  (define (next n) (if (= n 2)
                       3
                       (+ n 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
