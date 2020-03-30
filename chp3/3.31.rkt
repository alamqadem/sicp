#lang sicp

(#%require (file "../util.rkt"))

;ex. 3.31
; an action registers something in the agenda, so that's why it
; needs to be executed immediately to take after effectively after
; the specified delay, the first time an action is triggered without
; waiting for the input to change