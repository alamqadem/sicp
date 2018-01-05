#lang sicp
(#%require (file "util.rkt"))

;ex. 2.5
 (define (cons a b)
   (* (fast-expt 2 a) (fast-expt 3 b)))
 (define (car c)
   (if (not (= (remainder c 2) 0))
       0
       (+ 1 (car (/ c 2)))))
(define (cdr c)
  (if (not (= (remainder c 3) 0))
      0
      (+ 1 (cdr (/ c 3)))))
