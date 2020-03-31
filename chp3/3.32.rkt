#lang sicp

(#%require (file "../util.rkt"))
; ex. 3.32

; The and gate add two procedures to the agenda one for which wire for which the value is changed
; changing the wires from 0, 1 to 1, 0 will insert two procedures in the agenda one that for the
; first change from 0 to 1 which will set the output as the result of (logical-and 1 1)
; and an other one for the second change from 1 to 0 for the second wire which will set the 
; output wire to result of (logical-and 1 0). Now executing these procedure in the last in first
; out order wil compute the wrong value for the output wire, since it will correspond to a partial
; state
