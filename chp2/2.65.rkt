#lang sicp

(#%require (file "../util.rkt") (file "sets_bintree.rkt") (file "binary_tree.rkt"))

;ex. 2.65
(define (tree->list2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size
             (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree
                elts
                left-size)))
          (let ((left-tree
                 (car left-result))
                (non-left-elts
                 (cdr left-result))
                (right-size
                 (- n (+ left-size 1))))
            (let ((this-entry
                   (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree
                     (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(define (union-set set1 set2)
  (define (union-list lst1 lst2)
    (cond ((null? lst1) lst2)
          ((null? lst2) lst1)
          (else
           (let
              ((elem1 (car lst1))
               (elem2 (car lst2)))
            (cond
              ((= elem1 elem2)
               (cons elem1
                     (union-list (cdr lst1) (cdr lst2))))
              ((< elem1 elem2)
               (cons elem1
                     (union-list (cdr lst1) lst2)))
              ((> elem1 elem2)
               (cons elem2
                     (union-list lst1 (cdr lst2)))))))))
(let ((set1-as-list (tree->list2 set1))
      (set2-as-list (tree->list2 set2)))
  (list->tree
   (union-list
    set1-as-list
    set2-as-list))))
(define (print-tree t)
  (define (print-aux prefix t)
    (if (null? t)
        (begin
          (prefix)
          (display "()")
          (newline))
        (let
            ((new-prefix 
              (lambda ()
                (begin
                  (prefix)
                  (display "   ")))))
          (begin
            (print-aux
             new-prefix
             (left-branch t))
            (prefix)
            (display (entry t))
            (newline)
            (print-aux
             new-prefix
             (right-branch t))))))
  (print-aux (lambda () (display "")) t))

(define (intersection-set set1 set2)
  (define (intersection-list lst1 lst2)
    (cond ((or (null? lst1)
               (null? lst2))
           nil)
          (else
           (let ((elem1 (car lst1))
                (elem2 (car lst2)))
            (cond ((= elem1 elem2)
                   (cons elem1
                         (intersection-list
                          (cdr lst1)
                          (cdr lst2))))
                  ((< elem1 elem2)
                   (intersection-list
                    (cdr lst1)
                    lst2))
                  ((> elem1 elem2)
                   (intersection-list
                    lst1
                    (cdr lst2))))))))
  (let ((set1-as-list (tree->list2 set1))
        (set2-as-list (tree->list2 set2)))
    (list->tree
     (intersection-list set1-as-list
                        set2-as-list))))
                       
  

