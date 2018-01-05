#lang sicp

;ex. 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

;a.
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

;b.
(define (total-weight mobile)
  (define (total-branch-weight branch)
    (cond ((null? branch)
           0)
          ((not (pair? (branch-structure branch)))
           (branch-structure branch))
          (else (total-weight (branch-structure branch)))))
  (if (null? mobile)
      0
      (+ (total-branch-weight (left-branch mobile))
         (total-branch-weight (right-branch mobile)))))
;c.
(define (mobile-balanced? tree)
  (define (test-hanging-rods left-branch right-branch)
    
    (define (torque branch)
      (* (branch-length branch)
         (branch-structure branch)))
    
    (define (torquable? branch)
      (not (pair? (branch-structure branch))))
    
    (cond ((and (torquable? left-branch)
                (torquable? right-branch))
           (= (torque left-branch)
              (torque right-branch)))
          
          ((and (not (torquable? left-branch))
                (not (torquable? right-branch)))
           (and
            (mobile-balanced? (branch-structure left-branch))
            (mobile-balanced? (branch-structure right-branch))))
          
          (else false)))
  
  (if (null? tree) true
      (test-hanging-rods (left-branch tree)
                         (right-branch tree))))
;d
;> (define (make-mobile left right) (cons left right))
;> (define (make-branch length structure) (cons length structure))
;> (define (left-branch mobile)
;    (car mobile))
;> (define (right-branch mobile)
;    (cdr mobile))
;> (define (branch-length branch)
;    (car branch))
;> (define (branch-structure branch)
;    (cdr branch))
