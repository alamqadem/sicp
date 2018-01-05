#lang sicp
(#%require (file "1.42.rkt"))

;ex. 2.6
(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (add m n)
  (compose m n))

