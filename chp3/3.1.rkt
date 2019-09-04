#lang sicp

(#%require (file "../util.rkt"))
;ex. 3.1

(define (make-accumulator initial-value)
  (lambda (x)
    (begin
      (set! initial-value (+ x initial-value))
      initial-value)))

(define A (make-accumulator 5))

(A 10)
;15

(A 10)
;25
