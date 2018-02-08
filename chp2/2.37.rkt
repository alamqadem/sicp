#lang sicp

(#%require (file "../util.rkt") (file "2.36.rkt"))

; ex. 2.37
(define (dot-product v w)
  (accumulate + 0
              (map * v w)))
              ;; (accumulate-n
              ;;  *
              ;;  1
              ;;  (list v w))))

(define (matrix-*-vector m v)
  (map
   (lambda (m-row)
     (dot-product m-row v))
   m))

(define (transpose m)
  (accumulate-n
   cons
   nil
   m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
     (lambda (m-row)
       (map
        (lambda (n-col)
          (dot-product m-row n-col))
        cols))
     m)))

