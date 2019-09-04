#lang sicp

(#%require (file "../util.rkt"))

;ex. 3.7

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

(define (make-joint bank-acc passwd-old passwd-new)
  (lambda (passwd m)
    (if (eq? passwd passwd-new)
        (bank-acc passwd-old m)
        (lambda (.args) "Incorrect password"))))

(define peter-acc
  (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 
              'open-sesame 
              'rosebud))
