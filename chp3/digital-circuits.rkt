#lang sicp

(#%require (file "../util.rkt"))

(define (make-wire) 
    nil)
(define (get-signal wire)
    0)
(define (set-signal! wire new-value)
    'ok)
(define (add-action! wire action)
    'ok)
(define (inverter input output)
    (define (inverter-input)
        (let ((new-value
                (logical-not (get-signal input))))
            (after-delay 
                inverter-delay
                (lambda ()
                    (set-signal! output new-value)))))
    (add-action! input inverter-input)
    'ok)

(define (logical-not s)
    (cond ((= s 0) 1)
          ((= s 1) 0)
          (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
    (define (and-action-procedure)
        (let ((new-value (logical-and a1 a2)))
            (after-delay
                and-gate-delay
                (lambda () 
                    (set-signal! output new-value)))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    'ok)

(define (logical-and s1 s2)
    (if (and (= s1 1) (= s2 2))
        1
        0))