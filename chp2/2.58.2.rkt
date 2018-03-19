#lang sicp

(#%require (file "../util.rkt") (file "deriv.rkt"))


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else
         (flatmap
          (lambda (e)
            (if (or (number? e)
                    (variable? e))
                (list e) ; preserve numbers and variables
                e)) ; flat sums and product
          (list a1 '+ a2)))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
            (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1)
              (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(define (+-is-a-top-operator? x)
  (define (top-operators exp)
    (flatmap
     (lambda (n-and-e)
       (cdr n-and-e))
     (filter
      (lambda (n-and-e)
        (even? (car n-and-e)))
      (map
       (lambda (i e) (list i e))
       (enumerate-interval 1 (length exp))
       exp))))
  (pair? (memq '+ (top-operators x))))

(define (sum? x)
  (and (pair? x) (+-is-a-top-operator? x)))

(define (process-single-elem exp)
  (if (null? (cdr exp)) (car exp) exp))

(define (addend s)
  (define (exp-before-+ e)
   (if (eq? (car e) '+)
       nil
       (cons (car e) (exp-before-+ (cdr e)))))
  (process-single-elem (exp-before-+ s)))
    

(define (augend s)
  (process-single-elem (cdr (memq '+ s))))


(define (product? x)
  (and (pair? x)
       (eq? (cadr x) '*)
       (not (+-is-a-top-operator? x))))

(define (multiplier p) (car p))

(define (multiplicand p)
  (process-single-elem (cddr p)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else
         (error "unknow expression type: DERIV" exp))))




