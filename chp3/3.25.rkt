#lang sicp

(#%require (file "../util.rkt"))

;ex. 3.25

(define (make-table)
    (let ((local-table (list '*table*)))

        ; Table: pair of K, (list of Record)
        ; Record: Table
        ; Record: pair of K, (V or null)

        ; lookup: list of K -> V or boolean
        (define (lookup keys)
            (define (lookup-aux keys subtables)
                (if (null? (cdr keys))
                    (let ((record
                            (assoc (car keys)
                                    subtables)))
                        (if record 
                            (cdr record) 
                            false))
                    (let ((subtable (assoc (car keys) 
                                           subtables)))
                        (if subtable
                            (lookup-aux (cdr keys) (cdr subtable))
                            false))))
            (if (null? keys)
                    (error "empty keys list: LOOKUP" local-table)
                    (lookup-aux keys (cdr local-table))))

        ; assoc: K -> list of Record -> V or boolean
        (define (assoc key records)
            (cond 
                  ;((not (list? records)) false)
                  ((null? records) false)
                  ((equal? key (caar records))
                    (car records))
                  (else (assoc key (cdr records)))))

        ; insert: list of K -> V -> 'ok
        (define (insert! keys value)            
            ; insert-aux!: list of K -> Table -> void
            ; effect: change the table to insert value at the right place
            (define (insert-aux! keys table)
                (let ((subtable (assoc (car keys) (cdr table))))
                    (if subtable
                        (if (null? (cdr keys))
                            (set-cdr! subtable value)
                            (insert-aux! (cdr keys) subtable))
                        (insert-keys! keys table))))

            ; insert-keys!: list of K -> Table -> void
            ; effect: insert all the keys in table, inserting also the last record (last keys, value)
            ; invariant (not (eq? keys null))
            (define (insert-keys! keys table)
                (if (null? (cdr keys))
                    (set-cdr! 
                        table
                        (cons (cons (car keys) value)
                              (cdr table)))
                    (let ((new-table (list (car keys))))
                        (set-cdr! 
                            table
                            (cons new-table
                                  (cdr table)))
                        (insert-keys! (cdr keys) new-table))))
            
            (if (null? keys)
                (error "Empty list of keys: INSERT!" keys)
                (begin
                    (insert-aux! keys local-table)
                    'ok)))
        
        ; print-table: void -> void
        ; effect: print a table on the screen
        (define (print-table)
            ; is-table?: Table -> boolean
            (define (is-table? table)
                (list? (cdr table)))
                
            ; print-subtable: Table -> void
            ; prints the given table or record on the screen
            (define (print-subtable cur-subtable)
                (if (is-table? cur-subtable)
                    (let ((subtables (cdr cur-subtable)))
                        (if (null? subtables)
                            (begin
                                (display (car cur-subtable))
                                (display ": ")
                                (newline))
                            (begin
                                (display (car cur-subtable))
                                (display ": ")
                                (newline)
                                (print-table-aux subtables))))
                    (print-record cur-subtable)))

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

(put '(math +) 43)
(put '(math -) 45)
(put '(math *) 42)

(put '(letters a) 97)
(put '(letters b) 98)
(put '(letters c) 0)

(put '(pi) 3.14)
(put '(log210) 3.25)
(put '(k1 k2 k3 k4) 'nested)
; (put '() 'a)

(print-table)

(get '(math +))
(get '(letters a))
; (get '())
(get '(letters))
(get '(k1 k2 k3 k4))
