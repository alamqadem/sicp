#lang sicp

(#%require (file "../util.rkt"))

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap
      (lambda (j)
        (map
         (lambda (k) (list i j k))
         (enumerate-interval 1 n)))
      (enumerate-interval 1 n)))
   (enumerate-interval 1 n)))


(define (sum-to-s-triples s n)
  (define (sum-to-s? t)
    (= (+ (car t) (cadr t) (caddr t)) s))
  (filter
   sum-to-s?
   (unique-triples n)))
