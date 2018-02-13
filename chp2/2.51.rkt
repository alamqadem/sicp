#lang sicp

(#%require (file "../util.rkt") (file "paint.rkt"))

;ex. 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0))))
      (lambda (frame)
        (lambda (screen)
          ((paint-right frame) ((paint-left frame) screen)))))))

(define (below-1 painter1 painter2)
  (rotate270
   (beside
    (rotate90 painter1)
    (rotate90 painter2))))
