#lang sicp
(#%require (file "huffman_encoding.rkt") (file "../util.rkt"))

(define ht (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))

(define msg '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(define msg-bits (encode msg ht))

(equal? (decode msg-bits ht) msg)
