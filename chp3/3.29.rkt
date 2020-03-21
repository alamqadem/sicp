#lang sicp

(#%require (file "../util.rkt"))

;ex. 3.29

(define (or-gate a1 a2 output)
    (let ((c (make-wire)))
        (and-gate a1 a2 c)
        (inverter c output)
        'ok))
    