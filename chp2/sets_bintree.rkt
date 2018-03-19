#lang sicp

(#%require (file "../util.rkt") (file "binary_tree.rkt"))
(#%provide element-of-set? adjoin-set)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (car set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))
                     

