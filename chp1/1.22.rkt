#lang sicp

(#%require (file "util.rkt"))

; ex. 1.22
 (define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))
 (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
 (define (report-prime elapsed-time)
    (display "***")
    (display elapsed-time))
 ;(define runtime current-milliseconds)
(define (search-for-first-n-primes start n)
    (define (search-for-first-n-primes-aux start count start-time)
      (define (test-and-continue tested-number)
        (if (prime? tested-number)
            (begin
              (newline)
              (display tested-number)
              (search-for-first-n-primes-aux (+ start 2) (- count 1) start-time))
            (search-for-first-n-primes-aux (+ start 2) count start-time)))
      (define (close-func)
        (begin
          (newline)
          (display "Total execution time: ")
          (display (- (runtime) start-time))))
    (cond ((= count 0) (close-func)) 
          ((even? start) (search-for-first-n-primes-aux (+ start 1) n start-time))
          (else (test-and-continue start))))
    (search-for-first-n-primes-aux start n (runtime)))
