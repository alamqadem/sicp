#lang sicp

(#%require (file "../util.rkt"))

(define (=zero? x) (apply-generic '=zero? x))

(define (install-scheme-number-=zero?-package)
  ;; interface to the rest of the system
  (put '=zero? 'scheme-number
       (lambda (x) (= x 0)))
  'done)

(define (install-rational-=zero?-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (=zero?-rational x)
    (=zero? (numer x)))
  ;; interface to the rest of the system
  (put '=zero? 'rational
       (lambda (x) (=zero?-rational x)))
  'done)

(define (install-complex-=zero?-package)
  ;; internal procedures
  (define (=zero?-complex z)
    (and (=zero? (real-part z))
         (=zero? (imag-part z)))
  ;; interface to the rest of the system
  (put '=zero? 'complex
       (lambda (z)
          (=zero?-complex z)))
  'done)

  
       




