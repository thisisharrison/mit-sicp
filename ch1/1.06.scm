(define (new-if predicate then-clause else-clause) 
    (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5) ; 5
(new-if (= 1 1) 0 5) ; 0

(define (sqrt-iter guess x) 
    (new-if (good-enough? guess x) 
            guess
            (sqrt-iter (improve guess x) x)))

(define (improve guess x) 
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(sqrt-iter 1 4)
; applicative ordering -> therefore sqrt-iter is evaluated recursively
; this results in an infinite recursion because the else-clause is always evaluated
; Only one of the two consequent expressions get evaluated when using if, while both of the consequent expressions get evaluated with new-if.