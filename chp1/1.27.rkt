#lang sicp

(#%require (file "util.rkt"))

;ex. 1.27
(define (fermat-number? n)
    (define (fermat-number-aux? a)
      (cond ((= n a) #t)
            ((= (expmod a n n) a) (fermat-number-aux? (+ a 1)))
            (else #f)))
    (fermat-number-aux? 1))
