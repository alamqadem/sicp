#lang sicp

(#%require (file "../util.rkt") (file "paint.rkt"))

;a
(define (wave frame)
  (let ((head (list
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
                 (make-vect 0.55 0)
                 (make-vect 0.6 0.1))
                (make-segment
                 (make-vect 0.6 0.1)
                 (make-vect 0.55 0.2))
                (make-segment
                 (make-vect 0.55 0.2)
                 (make-vect 0.55 0.3))))
        (smile (list
                (make-segment
                 (make-vect 0.48 0.15)
                 (make-vect 0.52 0.15))
                (make-segment
                 (make-vect 0.47 0.12)
                 (make-vect 0.48 0.15))
                (make-segment
                 (make-vect 0.54 0.12)
                 (make-vect 0.52 0.15))
                ))
        (right-arm (list
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
                     (make-vect 0.35 0.4))))
        (left-arm (list
                   (make-segment
                    (make-vect 0.55 0.3)
                    (make-vect 0.7 0.3))
                   (make-segment
                    (make-vect 0.7 0.3)
                    (make-vect 1 0.5))
                   (make-segment
                    (make-vect 0.6 0.4)
                    (make-vect 1 0.6))))
        (right-leg (list
                    (make-segment
                     (make-vect 0.4 0.65)
                     (make-vect 0.2 1))
                    (make-segment
                     (make-vect 0.5 0.65)
                     (make-vect 0.3 1))))
        (left-leg (list
                   (make-segment
                    (make-vect 0.6 0.6)
                    (make-vect 0.85 1))
                   (make-segment
                    (make-vect 0.5 0.65)
                    (make-vect 0.75 1))))
        (body (list
               (make-segment
                (make-vect 0.35 0.4)
                (make-vect 0.35 0.65))
               (make-segment
                (make-vect 0.6 0.4)
                (make-vect 0.6 0.6)))))
    ((segment->painter
      (append
       head
       smile
       right-arm
       left-arm
       right-leg
       left-leg
       body))
     frame)))

;b

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))

;c
(define (square-limit1 painter n)
  (square-limit (flip-horiz painter) n))

(define (test painter size) 
  (let ((s (make-screen size))
        (f (make-frame (make-vect 0 0) (make-vect (- size 1) 0) (make-vect 0 (- size 1)))))
    (display-screen ((painter f) s))))



