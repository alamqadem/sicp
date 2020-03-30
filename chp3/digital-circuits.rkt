#lang sicp

(#%require (file "../util.rkt") (file "queue.rkt"))
(#%provide  ; wire
            make-wire get-signal set-signal! add-action! 
            ; agenda
            make-agenda current-time empty-agenda? add-to-agenda! remove-first-agenda-item! 
            first-agenda-item 
            ; time events
            after-delay propagate
            ; digital circuits
            inverter and-gate or-gate half-adder full-adder)

; ---------------------------------- wire ----------------------------------------------------
(define (make-wire) 
    (let ((signal-value 0)
          (action-procedures '()))
        (define (set-my-signal! new-value)
            (if (not (= signal-value new-value))
                (begin
                    (set! signal-value new-value)
                    (call-each
                        action-procedures))
                'done))   
        (define (accept-action-procedure! proc)
            (set! action-procedures 
                (cons proc action-procedures))
            (proc))
        (define (dispatch m)
            (cond ((eq? m 'get-signal)
                    signal-value)
                  ((eq? m 'set-signal!)
                    set-my-signal!)
                  ((eq? m 'add-action!)
                    accept-action-procedure!)
                  (else 
                    (error "Unknown operation: WIRE" m))))
    dispatch))

(define (get-signal wire) 
    (wire 'get-signal))

(define (set-signal! wire new-value)
    ((wire 'set-signal!) new-value))
    
(define (add-action! wire action)
    ((wire 'add-action!) action))

(define (call-each procedures)
    (if (null? procedures)
        'done
        (begin ((car procedures))
               (call-each (cdr procedures)))))

; ----------------------------- agenda -------------------------------------------------------

(define (make-time-segment time queue)
    (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
    (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
    (set-cdr! agenda segments))
(define (first-segment agenda)
    (car (segments agenda)))
(define (rest-segments agenda)
    (cdr (segments agenda)))
(define (empty-agenda? agenda)
    (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
    (define (belongs-before? segments)
        (or (null? segments)
            (< time
                (segment-time (car segments)))))

    (define (make-new-time-segment time action)
        (let ((q (make-queue)))
            (insert-queue! q action)
            (make-time-segment time q)))

    (define (add-to-segments! segments)
        (if (= (segment-time (car segments)) time)
            (insert-queue! 
                (segment-queue (car segments))
                action)
            (let ((rest (cdr segments)))
                (if (belongs-before? rest)
                    (set-cdr!
                        segments
                        (cons (make-new-time-segment
                                time
                                action)
                               (cdr segments)))
                    (add-to-segments! rest)))))

    (let ((segments (segments agenda)))
        (if (belongs-before? segments)
            (set-segments!
                agenda
                (cons (make-new-time-segment
                        time
                        action)
                       segments))
            (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
    (let ((q (segment-queue 
                    (first-segment agenda))))
        (delete-queue! q)
        (if (empty-queue? q)
            (set-segments!
                agenda
                (rest-segments agenda)))))

(define (first-agenda-item agenda)
    (if (empty-agenda? agenda)
        (error "Agenda is empty: FIRST-AGENDA-ITEM ")
        (let ((first-seg
                (first-segment agenda)))
            (set-current-time!
                agenda
                (segment-time first-seg))
            (front-queue
                (segment-queue first-seg)))))

; --------------------------- time events ------------------------------------------------------

(define (after-delay delay action)
    (add-to-agenda!
        (+ delay (current-time the-agenda))
        action
        the-agenda))

(define (propagate)
    (if (empty-agenda? the-agenda)
        'done
        (let ((first-item 
                (first-agenda-item the-agenda)))
            (first-item)
            (remove-first-agenda-item! the-agenda)
            (propagate))))

; ----------------------------digital circuits--------------------------------------------------
(define (inverter input output)
    (define (inverter-input)
        (let ((new-value
                (logical-not (get-signal input))))
            (after-delay 
                inverter-delay
                (lambda ()
                    (set-signal! output new-value)))))
    (add-action! input inverter-input)
    'ok)

(define (logical-not s)
    (cond ((= s 0) 1)
          ((= s 1) 0)
          (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
    (define (and-action-procedure)
        (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
            (after-delay
                and-gate-delay
                (lambda () 
                    (set-signal! output new-value)))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    'ok)

(define (logical-and s1 s2)
    (cond ((and (= s1 1) (= s2 1)) 
            1)
          ((or (= s1 0) (= s2 0)) 
            0)
          (else (error "Invalid signals" s1 s2))))

(define (or-gate a1 a2 output)
    (define (or-action-procedure)
        (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
            (after-delay
                or-gate-delay
                (lambda () 
                    (set-signal! output new-value)))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok)

(define (logical-or s1 s2)
    (cond ((or (= s1 1) (= s2 1)) 
            1)
          ((and (= s1 0) (= s2 0)) 
            0)
          (else (error "Invalid signals" s1 s2))))

; (define (or-gate a1 a2 output)
;     (let ((c (make-wire)))
;         (and-gate a1 a2 c)
;         (inverter c output)
;         'ok))

(define (half-adder a b s c)
    (let ((d (make-wire)) (e (make-wire)))
        (or-gate a b d)
        (and-gate a b c)
        (inverter c e)
        (and-gate d e s)
        'ok))

(define (full-adder a b c-in sum c-out)
    (let ((c1 (make-wire))
          (c2 (make-wire))
          (s (make-wire)))
        (half-adder b c-in s c1)
        (half-adder a s sum c2)
        (or-gate c1 c2 c-out)
        'ok))

(define (probe name wire)
    (add-action!
        wire
        (lambda ()
            (newline)
            (display name)
            (display " ")
            (display (current-time the-agenda))
            (display " New value = ")
            (display (get-signal wire))
            (newline))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

; (probe 'input-1 input-1)
; (probe 'input-2 input-2)

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)

(propagate)

(set-signal! input-2 1)

(propagate)
