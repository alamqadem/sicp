#lang sicp

(#%require (file "../util.rkt"))

;ex. 3.17

(define (count-pairs x)
  (define (iter seen x)
    (define (already-seen? x)
      (memq x seen))
    (if (or (not (pair? x))
            (already-seen? x))
        0
        (+ (iter (cons x seen) (car x))
           (iter (cons x seen) (cdr x))
           1)))
  (iter '() x))

