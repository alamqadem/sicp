#lang sicp

(#%require (file "../util.rkt"))

(define (equ? x y) (apply-generic 'equ? x y))

(define (install-scheme-number-equ?-package)
  ;; interface to the rest of the system
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  'done)

(define (install-rational-equ?-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (equ?-rational x y)
  (equ? (mul (numer x) (denom y))
        (mul (numer y) (denom x))))
  ;; interface to the rest of the system
  (put 'equ? '(rational rational)
       (lambda (x y) (equ?-rational x y)))
  'done)

(define (install-complex-equ?-package)
  ;; internal procedures
  (define (equ?-complex z1 z2)
    (and (equ? (real-part z1) (real-part z2))
         (equ? (imag-part z1) (imag-part z2))))
  ;; interface to the rest of the system
  (put 'equ? '(complex complex)
       (lambda (z1 z2)
          (equ?-complex z1 z2)))
  'done)

  
       




