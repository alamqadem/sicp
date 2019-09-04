#lang sicp

(#%require (file "../util.rkt"))

;ex. 3.2

(define (make-monitored f)
  (let ((calls-count 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?)
             calls-count)
            ((eq? arg 'reset-count)
             (set! calls-count 0))
            (else
             (begin
               (set! calls-count (+ calls-count 1))
               (f arg)))))))

(define s (make-monitored sqrt))

(s 100)
;10

(s 'how-many-calls?)
;1
