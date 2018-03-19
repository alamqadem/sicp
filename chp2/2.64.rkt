#lang sicp

(#%require (file "../util.rkt") (file "binary_tree.rkt"))

;ex. 2.64
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
;1.
; assumption: ordered list
; base case: n = 0. empty-tree as a result and return all the elements
; take the first half of the list, these elements will go in the left-subtree
; we have a subproblem that this the same as the original one, so we can use recurrence.
; The element after the ones that goes in the left subtree will be the element in the root
; node and the remaining elements will go in the right-subtree using recurrence as for
; the left-subtree, at the end we can construct the resulting tree and return the remaining
; elements.           
(define test
  (list->tree '(1 3 5 7 9 11)))
;      5
;    /   \
;   1     9
;  / \   / \
;     3 7   11

;2. O(n*log_2(n))
