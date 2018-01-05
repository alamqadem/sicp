#lang sicp

(#%require (file "2.2.rkt"))

;ex. 2.3

 (define (perimeter-rect r)
    (* 2 (+ (width-rect r) (height-rect r))))
 (define (area-rect r)
    (* (width-rect r) (height-rect r)))

(define (make-rect lowleft-point highright-point)
  (cons lowleft-point highright-point))
(define (width-rect r)
  (let ((lowleft-p (car r))
        (highright-p (cdr r)))
    (abs
     (- (x-point highright-p)
        (x-point lowleft-p)))))
(define (height-rect r)
  (let ((lowleft-p (car r))
        (highright-p (cdr r)))
    (abs
     (- (y-point highright-p)
        (y-point lowleft-p)))))

 ;; other implementation
 ;; (define (make-rect width height)
 ;;    (cons width height))
 ;; (define (width-rect r)
 ;;    (car r))
 ;; (define (height-rect r)
 ;;    (cdr r))
