#lang sicp

(#%require (file "../util.rkt"))

;ex. 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)

(define (adjoin-position new-row column rest-of-queens)
  (append rest-of-queens (list (make-position new-row column))))

(define (make-position row column)
  (cons row column))
(define (row-position pos)
  (car pos))
(define (column-position pos)
  (cdr pos))

(define (same-row? p1 p2) (= (row-position p1) (row-position p2)))
(define (same-diagonal? p1 p2)
  (=
   (abs (- (row-position p1) (row-position p2)))
   (abs (- (column-position p1) (column-position p2)))))

(define (safe? column positions)
  (define (k-th-queen? p) (= (column-position p) column))
  (define (queens-check? p1 p2)
    (or (same-row? p1 p2) (same-diagonal? p1 p2)))
  (define (check-others? p)
    (accumulate
     (lambda (x y) (or x y))
     false
     (map
      (lambda (pos)
        (and (not (k-th-queen? pos)) (queens-check? pos p)))
      positions)))
  (accumulate
   (lambda (x y) (and x y))
   true
   (map
    (lambda (queen-pos)
      (or
       (not (k-th-queen? queen-pos))
       (not (check-others? queen-pos))))
    positions)))
