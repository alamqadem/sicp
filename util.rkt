#lang sicp
(#%provide square average cube even? prime? fast-prime? divides? expmod sum fixed-point
           tolerance dx fixed-point-of-transform fast-expt average-damp
           add-rat sub-rat mul-rat div-rat equal-rat? numer denom print-rat make-rat
           add-interval mul-interval print-interval div-interval make-interval lower-bound
           upper-bound
           accumulate filter
           fold-left fold-right
           enumerate-interval flatmap)
(define (square x) (* x x))
(define (average x y)
  (/ (+ x y) 2))
(define (cube x) (* x x x))

(define (count-change amount)
  (define (cc amount kinds-of-coins)
    (define (first-denomination)
      (cond ((= kinds-of-coins 1) 1)
            ((= kinds-of-coins 2) 5)
            ((= kinds-of-coins 3) 10)
            ((= kinds-of-coins 4) 25)
            ((= kinds-of-coins 5) 50)))
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          ((= kinds-of-coins 1)
           (if (= (modulo amount (first-denomination))
                  0)
               1
               0))
          (else (+ (cc amount
                       (- kinds-of-coins 1))
                   (cc (- amount
                          (first-denomination))
                       kinds-of-coins)))))
  (cc amount 5))


; sec 1.2.4
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))

;sec. 1.2.6
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;sec. 1.3.1
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
;sec 1.3.3
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Value are not of opposite sign" a b)))))
;; (half-interval-method sin 2.0 4.0)
;; 3.14111328125
;; (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
;;                      1.0
;;                      2.0)
;; 1.89306640625
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
;; (fixed-point cos 1.0)
;; 0.7390822985224024
;; (fixed-point (lambda (y) (+ (sin y) (cos y)))
;;             1.0)
;; 1.2587315962971173
(define (sqrt-1 x)
  (fixed-point (lambda (y) (/ x y))
               1.0))
(define (sqrt-2 x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

; sec 1.3.4
(define (average-damp f)
  (lambda (x) (average x (f x))))
;; ((average-damp square) 10)
;55
(define (sqrt-3 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)

;; ((deriv cube) 5)
;75.00014999664018

(define (newton-transform g)
            (lambda (x) (- x (/ (g x) ((deriv g) x)))))
        (define (newton-method g guess)
            (fixed-point (newton-transform g) guess))

        (define (sqrt-4 x)
           (newton-method
            (lambda (y) (- (square y) x)) 1.0))

        (define (fixed-point-of-transform g transform guess)
            (fixed-point (transform g) guess))

       (define (sqrt-5 x)
           (fixed-point-of-transform
            (lambda (y) (/ x y)) average-damp 1.0))

       (define (sqrt-6 x)
         (fixed-point-of-transform
          (lambda (y) (- (square y) x)) newton-transform 1.0))

;sec. 2.1.1
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
;> (define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
;sec 2.1.4
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (print-interval x)
  (newline)
  (display "[")
  (display (lower-bound x))
  (display ", ")
  (display (upper-bound x))
  (display "]"))             

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)
(define (filter test? l)
  (if (null? l)
      nil
      (if (test? (car l))
          (cons (car l) (filter test? (cdr l)))
          (filter test? (cdr l)))))
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


       
