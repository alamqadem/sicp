#lang sicp

(#%require "util.rkt")

;ex. 1.32
;1.
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
(define (sum term a next b)
  (define (add a b)
    (+ a b))
  (accumulate add 0 term a next b))
(define (product term a next b)
  (define (mul a b)
    (* a b))
  (accumulate mul 1 term a next b))

; 2.
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))
