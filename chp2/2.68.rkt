#lang sicp

(#%require (file "../util.rkt") (file "huffman_encoding.rkt"))

(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (define (encode-aux prefix tree)
    (cond ((leaf? tree)
             (if (eq? (symbol-leaf tree)
                     sym)
                 prefix
                 false))
          ((memq sym (symbols tree))
           (let ((left-res (encode-aux (append prefix (list 0))
                                       (left-branch tree))))
             (if (not left-res)
                 (encode-aux (append prefix (list 1))
                             (right-branch tree))
                 left-res)))
          (else
           (error "bad tree: symbol not found in tree" sym))))
  (encode-aux '() tree))
           

