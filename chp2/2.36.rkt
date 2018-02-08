#lang sicp

(#%require (file "../util.rkt"))
(#%provide accumulate-n)

;ex. 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
