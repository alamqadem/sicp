#lang sicp

(#%require (file "../util.rkt"))

;ex. 2.60
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons
          (car set1)
          (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (cons set1 set2))))

;element-of-set: same complexity O(n)
;adjoin-set: O(1)
;intersection-set: same complexity O(n^2)
;union-set: O(1)

;This representation is better if you don't store a lot of element in the set
