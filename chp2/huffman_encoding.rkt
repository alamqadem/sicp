#lang sicp

(#%require (file "../util.rkt"))
(#%provide make-leaf make-code-tree decode left-branch right-branch leaf? symbol-leaf symbols make-leaf-set encode generate-huffman-tree)
;Huffman encoding trees

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (and (pair? object) (not (null? object))
       (eq? (car object) 'leaf)))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))


(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch
                (car bits)
                current-branch)))
          (if (leaf? next-branch)
              (cons
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits)
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair) ;symbol
                    (cadr pair));frequency
         (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (define (encode-aux prefix tree)
    (if (leaf? tree)
        (if (eq? (symbol-leaf tree)
                 sym)
            prefix
            '())
        (let ((left-res (encode-aux (append prefix (list 0))
                                    (left-branch tree))))
          (if (null? left-res)
                (encode-aux (append prefix (list 1))
                            (right-branch tree))
              left-res))))
  (let ((res (encode-aux '() tree)))
    (if (null? res)
        (error "bad tree: symbol not found in tree" sym)
        res)))

(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

(define (successive-merge leaves)
  (define (successive-merge-aux tree leaves1)
    (cond ((empty? leaves1) ; aka empty
           tree)
          ((null? tree)
           (successive-merge-aux
            (car leaves1)
            (cdr leaves1)))
          (else
           (successive-merge-aux
            (make-code-tree
             tree
             (car leaves1))
            (cdr leaves1)))))
  (successive-merge-aux '() leaves))

(define (empty? leaves)
  (null? leaves))

(define (one-leaf? leaves)
  (and (not (empty? leaves))
       (empty? (cdr leaves))))
          
