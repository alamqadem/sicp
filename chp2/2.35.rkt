#lang sicp

(#%require (file "../util.rkt"))

;ex. 2.35
(define (count-leaves t)
    (accumulate
     (lambda (subtree counted-leaves)
       (+
        counted-leaves
        subtree))
     0
     (map
      (lambda (subtree)
        (if (not (pair? subtree))
            1
            (count-leaves subtree)))
      t)))

