#lang sicp

(#%require (file "../util.rkt"))

;ex. 3.22

(define (make-queue)
    (let ((front-ptr '())
          (rear-ptr '()))

    (define (set-front-ptr! item)
        (set! front-ptr item))

    (define (set-rear-ptr! item)
        (set! rear-ptr item))

    (define (empty-queue?)
        (null? front-ptr))

    (define (front-queue)
        (if (empty-queue?)
            (error "FRONT called with an empty queue" '())
            (car (front-ptr))))

    (define (insert-queue! item)
        (let ((new-pair (cons item '())))
            (cond ((empty-queue?)
                    (set-front-ptr! new-pair)
                    (set-rear-ptr! new-pair))
                  (else (set-cdr! rear-ptr new-pair)
                        (set-rear-ptr! new-pair)))))

    (define (delete-queue!)
        (cond ((empty-queue?)
                (error "DELETE! called with an empty queue" '()))
                (else (set-front-ptr! (cdr front-ptr)))))

    (define (print-queue)
        (cond ((empty-queue?)
                    (display '())
                    (newline))
                (else
                    (display front-ptr)
                    (newline))))

    (define (dispatch m)
        (cond ((eq? m 'front-queue) (front-queue))
              ((eq? m 'empty-queue?) (empty-queue?))
              ((eq? m 'insert-queue!) (lambda (item) (insert-queue! item)))
              ((eq? m 'delete-queue!) (delete-queue!))
              ((eq? m 'print-queue) (print-queue))
              (else (error "Undefined operation MAKE-QUEUE" m))))
    dispatch))

(define (print-queue queue) (queue 'print-queue))
(define (insert-queue! queue item) 
    ((queue 'insert-queue!) item)
    queue)
(define (delete-queue! queue) 
    (queue 'delete-queue!)
    queue)


(define q1 (make-queue))

(print-queue q1)

(insert-queue! q1 'a)

(print-queue q1)

(insert-queue! q1 'b)

(print-queue q1)

(delete-queue! q1)

(print-queue q1)

(delete-queue! q1)

        
  
