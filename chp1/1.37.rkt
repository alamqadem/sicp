#lang sicp

(#%require (file "util.rkt"))

(#%provide cont-frac)

  ;ex. 1.37
  ;1.
  (define (cont-frac n d k)
    (define (try i)
      (if (> i k)
          1
          (/ (n i) (+ (d i) (try (+ i 1))))))
    (try 1))
  ;; (cont-frac (lambda (i) 1.0)
  ;;            (lambda (i) 1.0)
  ;;            10)
  ;; ;0.6180555555555556

  ;2.
  (define (cont-frac-iter n d k)
    (define (try i result)
      (if (< i 1)
          result
          (try (- i 1) (/ (n i) (+ (d i) result)))))
    (try k 1))

