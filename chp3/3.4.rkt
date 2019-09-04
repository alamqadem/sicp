#lang sicp

(#%require (file "../util.rkt"))

;ex. 3.4

(define (make-account balance password)
  (let ((wrong-password 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops . args)
      "Calling the cops!")
    (define (dispatch passwd m)
      (if (eq? password passwd)
          (begin
            (set! wrong-password 0)
            (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request: MAKE-ACCOUNT" m))))
          (begin
            (set! wrong-password (+ wrong-password 1))
            (if (>= wrong-password 7)
                call-the-cops
                (lambda (.args) "Incorrect password")))))
  dispatch))


(define acc 
  (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
;60

((acc 'some-other-password 'deposit) 50)
;"Incorrect password"
