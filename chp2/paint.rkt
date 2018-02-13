#lang sicp

(#%require (file "../util.rkt"))
(#%provide segment->painter make-screen display-screen
           make-segment make-vect
           make-frame
           transform-painter
           flip-vert shrink-to-upper-right rotate90 squash-inwards beside flip-horiz rotate180
           rotate270 below right-split up-split corner-split square-limit
           outline-painter x-painter diamond-painter wave)

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect scale-factor v)
  (make-vect (* scale-factor (xcor-vect v))
             (* scale-factor (ycor-vect v))))

(define (sub-vect v1 v2)
  (add-vect v1
            (scale-vect -1 v2)))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (cadr (cdr f)))

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (segment->painter segment-list)
  (lambda (frame)
    (accumulate
     compose
     identity
     (map
      (lambda (segment)
        (draw-line
         ((frame-coord-map frame)
          (start-segment segment))
         ((frame-coord-map frame)
          (end-segment segment))))
      segment-list))))


(define (make-screen size)
  (define (make-row size)
    (if (= size 0)
        nil
        (cons " " (make-row (- size 1)))))
  (define (make-rows rows-number)
    (if (= rows-number 0)
        nil
        (cons (make-row size) (make-rows (- rows-number 1)))))
  (make-rows size))

(define (size-screen s)
  (length s))

(define (display-screen s)
  (define (display-row r)
    (for-each
     (lambda (point) (begin
                       (display " ")
                       (display point)))
     r))
  (begin
    (for-each
     (lambda (row) (begin
                     (newline)
                     (display-row row)))
     s)
    (newline)))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (setpoint-screen p s)
  (let ((x-p (x-point p))
        (y-p (y-point p))
        (screen-size (size-screen s)))
    (define (setcolumn x cols)
      (cond ((null? cols) nil)
            ((= (round x) (round x-p))
             (cons "." (setcolumn (+ x 1) (cdr cols))))
            (else (cons (car cols) (setcolumn (+ x 1) (cdr cols))))))
    (define (setrow y rows)
      (cond ((null? rows) nil)
            ((= (round y) (round y-p))
             (cons (setcolumn 0 (car rows))
                   (setrow (+ y 1) (cdr rows))))
            (else (cons (car rows)
                        (setrow (+ y 1) (cdr rows))))))
    (setrow 0 s)))

(define (point-sequence start-v end-v)
  (define (iter current-v result)
    (let ((x-diff (- (xcor-vect end-v)
                     (xcor-vect start-v)))
          (y-diff (- (ycor-vect end-v)
                     (ycor-vect start-v))))
      (let ((move (make-vect
                   (/ x-diff (max (abs x-diff) (abs y-diff)))                             
                   (/ y-diff (max (abs x-diff) (abs y-diff))))))
        (define end-reached?
          (and ((if (< (xcor-vect move) 0) <= >=)
                (xcor-vect current-v) (xcor-vect end-v))
               ((if (< (ycor-vect move) 0) <= >=)
                (ycor-vect current-v) (ycor-vect end-v))))
        (if end-reached?
            (append result (list end-v))
            (iter
             (add-vect current-v move)
             (append result (list current-v)))))))
  (iter start-v nil))

(define (draw-line start-v end-v)
  (lambda (screen)
    (define (iter vect-list result)
      (if (null? vect-list)
          result
          (iter (cdr vect-list)
                (setpoint-screen
                 (car vect-list) result))))
    (iter
     (point-sequence start-v end-v)
     screen)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (shrink-to-upper-right painter)
  (transform-painter
   painter (make-vect 0.5 0.5)
   (make-vect 1.0 0.5) (make-vect 0.5 1.0)))
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
        (lambda (screen)
          ((paint-right frame) ((paint-left frame) screen)))))))


(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (rotate180 painter)
  (rotate90 (rotate90 painter)))
(define (rotate270 painter)
  (rotate90 (rotate180 painter)))

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

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter)
                        quarter)))
      (below (flip-vert half) half))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right 
                                   right))
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))
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
