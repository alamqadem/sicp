#lang sicp

(#%require (file "util.rkt"))

;ex.1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                    (filtered-accumulate filter combiner null-value term (next a) next b))
          (filtered-accumulate filter combiner null-value term (next a) next b))))
;a.
(define (sum-prime-squares a b)
  (define (add a b) (+ a b))
  (filtered-accumulate prime? add 0 square a inc b))
;b.
(define (mul-relative-primes n)
  (define (relative? x)
    (and (< x n) (= (gcd x n) 1)))
  (define (mul a b) (* a b))
  (filtered-accumulate relative? mul 1 identity 0 inc n))
