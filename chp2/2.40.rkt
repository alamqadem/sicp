#lang sicp

(#%require (file "../util.rkt"))

;ex. 2.40
(define (enumerate-interval start end)
  (if (> start end)
      nil
      (cons
       start
       (enumerate-interval
        (+ start 1)
        end))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map
      (lambda (j) (list i j))
      (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))
  (define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
