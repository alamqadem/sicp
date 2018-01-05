#lang sicp

(#%require (file "util.rkt"))

;ex. 2.1
(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (if (< (* n d) 0) -1 1)))
    (cons (* (/ (abs n) g) sign) (/ (abs d) g))))
