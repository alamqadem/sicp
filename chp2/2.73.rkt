#lang sicp

(#%require (file "../util.rkt"))

;ex. 2.73

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp)
               var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;1. You can if you consider number and variables operators with 0 operands

n;2.
(define (install-sum-product-package)
  ;; internal procedures
  
  (define (deriv-sum operands var)
    (if (and (list? operands)
             (not (null? operands))
             (not (null? (cdr operands))))
        (let ((addend (car operands))
              (augend (cadr operands)))
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))
        (error "deriv: Not enough operands " operands)))
  (define (deriv-product operands var)
    (if (and (list? operands)
             (not (null? operands))
             (not (null? (cdr operands))))
        (let ((multiplier (car operands))
              (multiplicand (cadr operands)))
          (make-sum
           (make-product
            multiplier
            (deriv multiplicand var))
           (make-product
            (deriv multiplier var)
            multiplicand)))
        (error "deriv: Not enough operands " operands)))
  ;; interface to the rest of the system
  (define (make-sum addend augend)
    (list '+ addend augend))
  (define (make-product multiplier multiplicand)
    (list '* multiplier multiplicand))
  (put 'deriv '(+) deriv-sum)
  (put 'deriv '(*) deriv-prod)

;3.
(define (install-exponent-package)
  ;; internal procedures
  (define (deriv-exponent operands var)
    (if (and (list? operands)
             (not (null? operands))
             (not (null? (cdr operands))))
        (let ((base (car operands))
              (exponent (cadr operands)))
          (make-product
           exponent
           (make-exponent
            base
            (make-subtraction exponent 1))))
        (error "deriv: Not enough operands " operands)))
  ;; interface to the rest of the system
  (define (make-exponent base exponent)
    (list '^ base exponent))
  (put 'deriv '(^) deriv-exponent))

;4.
;(put '(+) 'deriv deriv-sum)
  
