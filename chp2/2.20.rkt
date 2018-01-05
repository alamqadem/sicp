#lang sicp

;ex. 2.20
(define (same-parity x . l)
  (let ((parity (remainder x 2)))
    (define (same-parity-aux l)
      (cond ((null? l) nil)     
            ((= (remainder (car l) 2) parity)
             (cons (car l) (same-parity-aux (cdr l))))
            (else
             (same-parity-aux (cdr l)))))
    (cons x
          (same-parity-aux l))))

