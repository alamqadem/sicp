#lang sicp

(#%require (file "../util.rkt") 
           (file "digital-circuits.rkt"))

;ex. 3.29
; or-gate-delay = and-gate-delay + 3*inverter-delay

(define (or-gate1 a1 a2 output)
    (let ((not-a1 (make-wire))
          (not-a2 (make-wire))
          (and-not-a1-not-a2 (make-wire)))
        (inverter a1 not-a1)
        (inverter a2 not-a2)
        (and-gate not-a1 not-a2 and-not-a1-not-a2)
        (inverter and-not-a1-not-a2 output)
        'ok))
    