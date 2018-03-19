#lang sicp

(#%require (file "../util.rkt") (file "deriv.rkt"))

;ex. 2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-exponentiation
           (base exp)
           (dec (exponent exp)))))
        (else
         (error "unknow expression type: DERIV" exp))))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))

(define (make-exponentiation base exponent)
  (if (not (number? exponent))
      (error "exponent should is not a number: UNSUPPORTED TYPE" exponent)
      (cond
        ((= exponent 0) 1)
        ((= exponent 1) base)
        (else 
         (list '** base exponent)))))


(define (base e) (cadr e))
(define (exponent e) (caddr e))






