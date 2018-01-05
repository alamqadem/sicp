#lang sicp

;ex. 2.28
(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? (car tree)))
         (cons (car tree) (fringe (cdr tree))))
        (else
         (append (fringe (car tree)) (fringe (cdr tree))))))

