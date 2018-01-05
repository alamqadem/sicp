#lang sicp
(#%require (file "util.rkt") (file "1.37.rkt"))

;ex. 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- 0.0 (square x))))
             (lambda (i)
               (- (* 2 i) 1))
             k))
