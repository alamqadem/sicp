#lang sicp

(#%require "util.rkt")

;ex. 1.31
;1.
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (factorial n)
  (product identity 1 inc n))
(define (pi-product a b)
  (define (pi-num-term x)
    (* 2 x))
  (define (pi-den-term x)
    (+ (* 2 x) 1))
  (/ (* (product pi-num-term a inc b) (product pi-num-term (+ a 1) inc (- b 1)))
     (* (product pi-den-term a inc (- b 1)) (product pi-den-term a inc (- b 1)))))
;2.
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

