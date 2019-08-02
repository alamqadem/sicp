#lang sicp

(#%require (file "../util.rkt") (file "huffman_encoding.rkt") (file "sets.rkt"))

;ex. 2.69
(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

(define (successive-merge leaves)
  (cond ((empty? leaves) ; aka empty
         '())
        (one-leaf? ; aka one-element
         leaves)
        (else
         (successive-merge
         (adjoin-set
          (car leaves)
          (successive-merge
           (cdr leaves)))))))

(define (empty? leaves)
  (null? leaves))

(define (one-leaf? leaves)
  (and (not (empty? leaves))
       (empty? (cdr leaves))))
         

         

      
