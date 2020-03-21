#lang sicp

(#%require (file "../util.rkt"))
(#%require (file "digital-circuits.rkt"))

;ex. 3.28

(define (or-gate a1 a2 output)
    (define (or-action-procedure)
        (let ((new-value (logical-or a1 a2)))
            (after-delay
                or-gate-delay
                (lambda ()
                    (set-signal! output new-value)))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure))

(define (logical-or s1 s2)
    (if (or (= s1 1) (= s2 1))
        1
        0))