#lang sicp
(#%require (file "1.43.rkt") (file "util.rkt"))
; ex. 1.44
(define (smooth f)
  (lambda (x) (/
               (+ (f (- x dx))
                  (f x)
                  (f (+ x dx)))
               3)))

(define (smooth-nfold f n)
  ((repeated smooth n) f))
