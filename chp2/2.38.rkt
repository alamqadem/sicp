#lang sicp

(#%require (file "../util.rkt"))


;ex. 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)

(fold-right / 1 (list 1 2 3))
;1 1/2
(fold-left / 1 (list 1 2 3))
;1/6
(fold-right list nil (list 1 2 3))
;(1 (2 (3 ())))
(fold-left list nil (list 1 2 3))
;(((() 1) 2) 3)
;op should satisfy the commutative property
