#lang sicp

(#%require (file "../util.rkt"))

;ex. 3.18
(define (cyclic-list? l)
  (define (iter seen l)
    (define (already-seen? x)
      (memq x seen))
    (if (null? l)
        #f
        (let ((x (car l)))
          (if (already-seen? x)
            #t
            (iter (cons x seen) (cdr l))))))
  (iter '() l))
           
            
