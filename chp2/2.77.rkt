#lang sicp

(#%require (file "../util.rkt"))

;ex. 2.77

; complex|. -> rectangular|. -> 3|4
; (magnitude complex|. -> rectangular|. -> 3|4)
; (apply-generic 'magnitude rectangular|. -> 3|4)
; (apply-generic 'magnitude 3|4)
; (magnitude-rectangular 3|4)
