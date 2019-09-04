#lang sicp

(#%require (file "../util.rkt"))

;ex. 3.3

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch passwd m)
    (if (eq? password passwd)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT" m)))
        (lambda (.args) "Incorrect password")))
  dispatch)

(define acc 
  (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
;60

((acc 'some-other-password 'deposit) 50)
;"Incorrect password"
