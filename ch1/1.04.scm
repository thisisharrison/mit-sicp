; The if statement returns either a - or a +, which is then applied to the operands.
; if b greater than 0, add a 
; if b less than 0, minus a 

; ((if (> b 0) + -) a b)
; if b > 0 then (+ a b)
; if b < 0 then (- a b)

(define (a-plus-abs-b a b) ((if (> b 0) + -) a b))

(a-plus-abs-b 3 -5) ; 8
(a-plus-abs-b 3 5) ; 8