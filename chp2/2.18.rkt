#lang sicp

;ex. 2.18
(define (reverse l)
  (define (reverse-iter l r)
    (if (null? (cdr l))
        (cons (car l) r)
        (reverse-iter (cdr l) (cons (car l) r))))
  (reverse-iter l nil))
