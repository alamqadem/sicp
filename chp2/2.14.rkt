#lang sicp

(#%require (file "util.rkt") (file "2.12.rkt"))

;2.14
(define r1
  (make-center-percent 3.5 0.04))
(define r2
  (make-center-percent 7.3 0.02))
(define (print-center-percent x)
  (newline)
  (display (center x))
  (display "+-")
  (display (percent x)))
(define one (make-interval 1 1))
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))
(print-center-percent
 (par1 r1 r2))

;2.373056383058201+-0.08629651388350064

(print-center-percent
 (par2 r1 r2))

;2.365533309006717+-0.03352377987078702
