#lang sicp

(#%require (file "util.rkt"))

;ex. 1.28
(define (expmod base exp m)
  (define (non-trivial n)
    (define (squared-remainder)
      (remainder (square n) m))
    (define (test-square s)
      (if (= s 1)
          0
          s))
    (cond ((or (= n 1) (= n (- m 1))) (squared-remainder))
          (else (test-square (squared-remainder)))))
  (cond ((= exp 0) 1)
        ((even? exp)
         (non-trivial (expmod base (/ exp 2) m)))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))
(define (milner-rabin-test n)
  (define (try-it a)
    (not (= (expmod a n n) 0)))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime-mr? n times)
  (cond ((= times 0) true)
        ((milner-rabin-test n) (fast-prime-mr? n (- times 1)))
        (else false)))
