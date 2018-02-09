#lang sicp

(#%require (file "../util.rkt"))


;ex. 2.46
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect scale-factor v)
  (make-vect (* scale-factor (xcor-vect v))
             (* scale-factor (ycor-vect v))))
(define (sub-vect v1 v2)
  (add-vect v1
            (scale-vect -1 v2)))
