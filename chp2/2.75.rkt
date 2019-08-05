#lang sicp

(#%require (file "../util.rkt"))

;ex. 2.75

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unkown op: MAKE-FROM-REAL-IMAG"
                  op))))
  dispatch)


(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          (else
           (error "Unkown op: MAKE-FROM-MAG-ANG"
                  op))))
  dispatch)

(define (apply-generic op arg) (arg op))
