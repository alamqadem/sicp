#lang sicp

(define (pascal-triangle level pos)
  (cond ((and (= level 0) (= pos 0)) 1)
        ((< pos 0) 0)
        ((> pos level) 0)
        (else
         (+ (pascal-triangle (- level 1) (- pos 1))
            (pascal-triangle (- level 1) pos)))))

