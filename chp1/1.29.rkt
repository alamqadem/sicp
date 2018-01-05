#lang sicp

(#%require (file "util.rkt"))

; ex. 1.29
(define (simpson-rule f a b n)
  (define (odd-term x)
    (* 4 (f x)))
  (define (even-term x)
    (* 2 (f x)))
  (define (calculate h)
    (define (next x)
      (+ x (* 2 h)))
    (* (+ (f a)
          (sum odd-term (+ a h) next (- b h))
          (sum even-term (+ a (* 2 h)) next (- b h))
          (f b))
       (/ h 3.0)))
  (calculate (/ (- b a) n)))
