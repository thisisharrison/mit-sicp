(define (p) (p)) 

(define (test x y)
    (if (= x 0) 0 y))

; normal order evaluation => fully expand then reduce
(test 0 (p))
; ((test 0 (p))
;     (if (#t) 0 (p)))
; 0

; applicative order evaluation => evaluate args then apply
; (test 0 (p)) -> initial call
; (test 0 (p)) -> called (p) and returned (p)
; (test 0 (p)) -> recusive call p again
; ...

; Lisp uses applicative order