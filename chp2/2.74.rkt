#lang sicp

(#%require (file "../util.rkt"))

;ex.2.74

;1.
(define (get-record division-file employee-name)
  (let ((division (car division-file))
        (div-file (cdr division-file)))
  (get division 'get-record (list div-file employee-name)))

; each division needs to implement a get-record that retrives the employee record based on the employee-name

;2.
(define (get-salary employee-record)
  (let
      ((division (get-attached-tag employee-record)))
    (get division 'get-salary (list employee-record))))
;the record needs to have the division attached as the type of the record

;3.
(define (find-employee-record divisions-files employee-name)
  (if (null? divisions-files)
      (error "Employee not found " employee-name)
      (let* ((division-file (car divisions-files))
             (employee-record (get-record division-file employee-name)))
        (if (null? employee-record)
            (find-employee-record (cdr divisions-files) employee-name)
            employee-record))))

;4.
;create a new division type and add a function get-record and get-salary for that division
                       

