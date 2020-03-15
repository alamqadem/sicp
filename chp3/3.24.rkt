#lang sicp

(#%require (file "../util.rkt"))
;ex. 3.24

(define (make-table)
    (let ((local-table (list '*table*)))
        (define (lookup key-1 key-2)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record
                            (assoc key-2 
                                   (cdr subtable))))
                        (if record (cdr record) false))
                    false)))

        (define (assoc key records)
            (cond ((null? records) false)
                  ((equal? key (caar records))
                    (car records))
                  (else (assoc key (cdr records)))))

        (define (insert! key-1 key-2 value)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record
                            (assoc key-2
                                    (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! 
                                subtable 
                                (cons (cons key-2 value)
                                      (cdr subtable)))))
                    (set-cdr! local-table 
                              (cons (list key-1
                                        (cons key-2 value))
                                    (cdr local-table)))))`
            'ok)

        (define (print-table)
            (define (print-subtable cur-subtable)
                (let ((records (cdr cur-subtable)))
                    (if (null? records)
                        (begin
                          (display (car cur-subtable))
                          (display ": ")
                          (newline))
                        (begin
                            (display (car cur-subtable))
                            (display ": ")
                            (newline)
                            (map print-record records)))))

            (define (print-record record)
                (let ((key (car record))
                      (value (cdr record)))
                      (display "\t")
                      (display key)
                      (display ": ")
                      (display value)
                      (newline)))

            (define (print-table-aux subtables)
                (if (null? subtables)
                    (newline)
                    (begin
                        (print-subtable (car subtables))
                        (newline)
                        (print-table-aux (cdr subtables)))))

            (print-table-aux (cdr local-table)))

        (define (dispatch m)
            (cond ((eq? m 'lookup-proc) lookup)
                  ((eq? m 'insert-proc!) insert!)
                  ((eq? m 'print-table) print-table)
                  (else (error "Unknown operation: TABLE" m))))
        dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define print-table (operation-table 'print-table))

operation-table

(display "calling print-table\n")
(print-table)

(put 'math '+ 43)
(put 'math '- 45)
(put 'math '* 42)

(put 'letters 'a 97)
(put 'letters 'b 98)

(print-table)

(get 'math '+)