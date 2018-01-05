#lang sicp
(#%require (file "util.rkt") (file "1.37.rkt"))

;Ex. 1.38
(+ (cont-frac (lambda (i) 1.0)
              (lambda (i)
                (let ((pos (remainder i 3)))
                  (cond ((or (= pos 0) (= pos 1))
                         1)
                        (else
                         (- i (round (/ i 3)))))))
              10)
   2)
