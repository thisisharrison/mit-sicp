(define (sqrt x)
    (sqrt-iter 1.0 x))

(define (sqrt-iter guess x) 
    (if (good-enough? guess x) 
        guess
        (sqrt-iter (improve guess x) x)))

(define (improve guess x) 
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

; ; original
; (define (good-enough? guess x)
;     (< (abs (- (square guess) x)) 0.001))

; modiified
(define (good-enough? guess x)
    ; stop when change is very small fraction of guess
    (< (abs (- (improve guess x) guess)) (* guess 0.001))) 

(sqrt 100.0)
(sqrt 0.00121)
