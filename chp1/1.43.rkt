#lang sicp
(#%require (file "1.42.rkt"))
(#%provide repeated)
;ex. 1.43
(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))
