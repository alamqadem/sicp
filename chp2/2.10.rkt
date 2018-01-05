#lang sicp
(#%require (file "util.rkt"))
;ex. 2.10
(define (div-interval x y)
      (if (or (= (upper-bound y) 0) (= (lower-bound y) 0))
          (error "Cannot divide by an interval that spans 0")
          (mul-interval
           x
           (make-interval (/ 1.0 (upper-bound y))
                          (/ 1.0 (lower-bound y))))))
