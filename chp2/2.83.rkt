#lang sicp

(#%require (file "../util.rkt"))

;ex. 2.83

(define (apply-generic op . args)
  
  (define (coerce-to type type-tags)
    (let ((coerce-procs
           (map (lambda (type-i)
                  (if (eq? type type-i)
                      (lambda (t) t)
                      (get-coercion type-i
                                    type)))
                type-tags)))
      (if (not (memq #f coerce-procs))
          (map
           (lambda (p x) (p x))
           coerce-procs args)
          #f)))
  
  (define (apply-generic-aux types-to-try type-tags)
    (if (null? types-to-try)
        (error 
         "No method for these types"
         (list 
          op 
          type-tags))
        (let ((type (car types-to-try)))
          (let ((coerced-args (coerce-to type type-tags))
                (coerced-types (map (lambda (i) (type)) args)))
            (if coerced-types
                (let ((proc (get op coerced-types)))
                  (if proc
                      (apply proc (map contents coerced-args))
                      (apply-generic-aux (cdr types-to-try) type-tags)))
                (apply-generic-aux (cdr types-to-try) type-tags))))))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (apply-generic-aux type-tags type-tags)))))
