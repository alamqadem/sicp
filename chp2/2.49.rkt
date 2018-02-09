#lang sicp

(#%require (file "../util.rkt") (file "paint.rkt"))

;ex. 2.49
(define (outline-painter frame)
  (let ((top-segment
         (make-segment
          (make-vect 0 0)
          (make-vect 0 1)))
        (bottom-segment
         (make-segment
          (make-vect 1 0)
          (make-vect 1 1)))
        (left-segment
         (make-segment
          (make-vect 0 0)
          (make-vect 1 0)))
        (right-segment
         (make-segment
          (make-vect 0 1)
          (make-vect 1 1))))
    (let ((segment-list
           (list bottom-segment
                 right-segment
                 top-segment
                 left-segment)))
      ((segment->painter segment-list) frame))))

(define (x-painter frame)
  ((segment->painter
    (list
     (make-segment
      (make-vect 0 0)
      (make-vect 1 1))
     (make-segment
      (make-vect 0 1)
      (make-vect 1 0)))) frame))

(define (diamond-painter frame)
  ((segment->painter
    (list
     (make-segment
      (make-vect 0.5 0)
      (make-vect 0 0.5))
     (make-segment
      (make-vect 0 0.5)
      (make-vect 0.5 1))
     (make-segment
      (make-vect 0.5 1)
      (make-vect 1 0.5))
     (make-segment
      (make-vect 0.5 0)
      (make-vect 1 0.5)))) frame))

(define (wave frame)
  ((segment->painter
    (list
     (make-segment
      (make-vect 0.45 0)
      (make-vect 0.4 0.1))
     (make-segment
      (make-vect 0.4 0.1)
      (make-vect 0.45 0.2))
     (make-segment
      (make-vect 0.45 0.2)
      (make-vect 0.45 0.3))
     (make-segment
      (make-vect 0.35 0.25)
      (make-vect 0.45 0.3))
     (make-segment
      (make-vect 0.35 0.25)
      (make-vect 0.2 0.35))
     (make-segment
      (make-vect 0.2 0.35)
      (make-vect 0 0.25))
     (make-segment
      (make-vect 0 0.35)
      (make-vect 0.2 0.45))
     (make-segment
      (make-vect 0.2 0.45)
      (make-vect 0.35 0.4))
     (make-segment
      (make-vect 0.35 0.4)
      (make-vect 0.35 0.65))
     (make-segment
      (make-vect 0.4 0.65)
      (make-vect 0.2 1))
     (make-segment
      (make-vect 0.5 0.65)
      (make-vect 0.3 1))
     (make-segment
      (make-vect 0.55 0)
      (make-vect 0.6 0.1))
     (make-segment
      (make-vect 0.6 0.1)
      (make-vect 0.55 0.2))
     (make-segment
      (make-vect 0.55 0.2)
      (make-vect 0.55 0.3))
     (make-segment
      (make-vect 0.55 0.3)
      (make-vect 0.7 0.3))
     (make-segment
      (make-vect 0.7 0.3)
      (make-vect 1 0.5))
     (make-segment
      (make-vect 0.6 0.4)
      (make-vect 1 0.6))
     (make-segment
      (make-vect 0.6 0.4)
      (make-vect 0.6 0.6))
     (make-segment
      (make-vect 0.6 0.6)
      (make-vect 0.85 1))
     (make-segment
      (make-vect 0.5 0.65)
      (make-vect 0.75 1))))
   frame))

