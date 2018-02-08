#lang sicp

(#%require (file "../util.rkt"))

;ex. 2.39
(define (reverse sequence)
   (fold-right (lambda (x y)
                 (if (null? y)
                     (cons x y)
                     (append y (list x))))
               nil
               sequence))
(define (reverse-1 sequence)
  (fold-left (lambda (x y)
               (cons y x))
             nil
             sequence))
