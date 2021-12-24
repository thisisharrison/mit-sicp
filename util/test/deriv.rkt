#lang racket
(require (submod "../math.rkt"))

(deriv '(+ x 3) 'x)
; (+ 1 0) = 1
(deriv '(* x y) 'x)
; (+ (* x 0) (* 1 y)) = y
(deriv '(* (* x y) (+ x 3)) 'x) 
; (+ (* (* x y) (+ 1 0))
;    (* (+ (* x 0) (* 1 y))
;       (+ x 3)))
; = (+ (* x y) (* y (+ x 3)))