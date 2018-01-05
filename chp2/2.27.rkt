#lang sicp

;ex. 2.27
(define (deep-reverse l)
  (define (deep-reverse-iter l r)
    (if (null? l)
        r
        (deep-reverse-iter (cdr l)
                           (cons
                            (if (pair? (car l))
                                (deep-reverse-iter (car l) nil)
                                (car l))
                            r))))
  (deep-reverse-iter l nil))
