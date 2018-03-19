#lang sicp

(#%require (file "../util.rkt") (file "sets_ord.rkt"))

;ex. 2.61
(define (adjoin-set x set)
  (cond ((null? set)
         (list x))
        ((= x (car set))
         (cdr set))
        ((< x (car set))
         (cons x set))
        ((> x (car set))
         (cons (car set)
               (adjoin-set x (cdr set))))))

