#lang sicp

;ex. 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (subset)
                        (cons (car s) subset))
                      rest)))))
; it works because in the inductive step if we have a set with all the subsets of rest the new subsets obtained adding the head element
; is the union of the head element with all the subsets and the set of subsets of rest
; the base case is correct so the algoritm works

