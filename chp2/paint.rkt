#lang sicp

(#%require (file "../util.rkt"))
(#%provide segment->painter make-screen display-screen
           make-segment make-vect
           make-frame)

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
