#lang sicp

(#%require (file "../util.rkt"))

;ex. 2.54

(define (equal? a b)
  (cond ((and (symbol? a) (symbol? b))
         (eq? a b))
        ((and (null? a) (null? b))
         true)
        ((and (pair? a) (pair? b))
         (and
          (equal? (car a) (car b))
          (equal? (cdr a) (cdr b))))
        (else false)))

(define test1
  (equal? '(this is a list) '(this is a list)))
(define test2
  (equal? '(this is a list) '(this (is a) list)))
