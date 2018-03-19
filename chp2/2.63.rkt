#lang sicp

(#%require (file "../util.rkt") (file "binary_tree.rkt"))
;ex. 2.63
(define (tree->list1 tree)
  (if (null? tree)
      '()
      ;; (begin
      ;;   (display "processing: ")
      ;;   (display (entry tree))
      ;;   (newline)
      (append
       (tree->list1
        (left-branch tree))
       (cons (entry tree)
             (tree->list1
                (right-branch tree))))))
;; )

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

;; 1. same result for all the trees
(define test
  (begin
    (tree->list1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
    ;; (mcons 1 (mcons 3 (mcons 5 (mcons 7 (mcons 9 (mcons 11 '()))))))
    (tree->list2 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
    ;; (mcons 1 (mcons 3 (mcons 5 (mcons 7 (mcons 9 (mcons 11 '()))))))
    (tree->list1 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
    ;; (mcons 1 (mcons 3 (mcons 5 (mcons 7 (mcons 9 (mcons 11 '()))))))
    (tree->list2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
    ;; (mcons 1 (mcons 3 (mcons 5 (mcons 7 (mcons 9 (mcons 11 '()))))))
    (tree->list1 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
    ;; (mcons 1 (mcons 3 (mcons 5 (mcons 7 (mcons 9 (mcons 11 '()))))))
    (tree->list2 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
    ;; (mcons 1 (mcons 3 (mcons 5 (mcons 7 (mcons 9 (mcons 11 '()))))))
    ))

;; 2. compl(tree->list1) = O(n*log_2(n))
;;    compl(tree->list2) = O(n)
;; tree->list2 grows more slowly because use cons (O(1)) instead of append (O(n))
