#lang sicp

(#%require (file "../util.rkt"))

;ex. 3.5

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((rect-area (* (abs (- x1 x2))
                      (abs (- y1 y2)))))
    (* rect-area (monte-carlo trials
                              (lambda ()
                                (P (random-in-range x1 x2) (random-in-range y1 y2)))))))

(define (random-in-range low high)
  (+ low (random high) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))
    
(define (estimate-pi trials)
  (exact->inexact (estimate-integral
          (lambda (x y) (< (+ (square x) (square y)) 1))
          -2 2 -2 2 trials)))
